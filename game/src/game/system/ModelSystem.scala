package game.system

import core._
import gfx._
import render._
import asset.ModelAsset
import game.lighting.LightProbe
import game.shader._
import render.Renderer.UniformRef

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object ModelSystem {

  val MaxInstancesPerDraw = 8

  object InstancedUniform extends UniformBlock("InstancedUniform") {
    val World = mat4x3("World", MaxInstancesPerDraw)
    val LightInfo = ivec4("LightInfo", MaxInstancesPerDraw)
  }

  // Note: Properties are `var` here in case of pooling!

  private class MeshInstance {
    var worldTransform: Matrix43 = Matrix43.Identity
    var lightProbe: LightProbe = null
    var next: MeshInstance = null
  }

  class ManualMeshDraw {
    /** Mesh to use */
    var mesh: Mesh = null

    /** World transorm matrix */
    var worldTransform: Matrix43 = null
  }

  class InstancedMeshDraw {
    /** Mesh to use */
    var mesh: Mesh = null

    /** Number of instances to draw */
    var num: Int = 0

    /** Reference to `InstancedUniform` */
    var instanceUbo: UniformRef = null

    /** Reference to `LightProbeUniform` */
    var lightProbeUbo: UniformRef = null
  }

  /**
    * A model instance in the world.
    */
  class ModelRef private[system] (val parent: Entity, private[system] val asset: ModelAsset) {
    private[system] var model: Model = asset.get
    private[system] var state: ModelState = new ModelState(model)
    private[system] var nodeRefs = Array[NodeRef]()
    private[system] var manualDrawList: ArrayBuffer[ManualMeshDraw] = null

    /** Draw this model manually using `manualDraws` */
    var useManualDraws: Boolean = false

    /**
      * Transform relative to the parent.
      */
    var transform: Matrix43 = Matrix43.Identity

    /**
      * Light probe the model uses.
      */
    var lightProbe: LightProbe = LightProbe.Empty

    /**
      * Find a node matching a name in the model.
      */
    def findNode(name: Identifier): NodeRef = {
      val ref = new NodeRef(this, name)
      nodeRefs :+= ref
      ref
    }

    /** Returns a list of transformed meshes for this model */
    def manualDraws: Seq[ManualMeshDraw] = if (manualDrawList == null)
      Array[ManualMeshDraw]()
    else
      manualDrawList
  }

  class NodeRef private[system](private[system] val ref: ModelRef, val name: Identifier) {
    private[system] var index = ref.model.findNodeByName(name)

    var localTransform: Matrix43 = Matrix43.Identity
  }

  private val EmptyModelRefs = Array[ModelRef]()
  private val entityModels = new mutable.HashMap[Entity, Array[ModelRef]].withDefaultValue(EmptyModelRefs)

  private val allModels = new ArrayBuffer[ModelRef]()
  private val visibleModels = new ArrayBuffer[ModelRef]()
  private val alwaysVisibleModels = new ArrayBuffer[ModelRef]()

  /**
    * Called when the asset load state has changed.
    * Re-resolves all models from assets.
    */
  def assetsLoaded(): Unit = {
    for (model <- allModels) {
      if (!model.model.loaded) {
        model.model = model.asset.get
        model.state = new ModelState(model.model)
        for (ref <- model.nodeRefs) {
          ref.index = model.model.findNodeByName(ref.name)
        }
      }
    }
  }

  /**
    * Shorthand for adding a model to the world with an asset name.
    * See the other overload for more documentation.
    */
  def addModel(parent: Entity, assetName: Identifier): ModelRef = addModel(parent, ModelAsset(assetName))

  /**
    * Add a model to the world that is parented to `parent`. This means that
    * the root transform of the model will follow the transform of the entity.
    * The model will also be deleted when the parent entity is deleted.
    *
    * @param parent Parent entity for the model.
    * @param asset Asset to use for the model.
    * @return A model reference that can be used to manipulate the model.
    */
  def addModel(parent: Entity, asset: ModelAsset): ModelRef = {
    val model = new ModelRef(parent, asset)

    allModels += model
    if (parent != null) {
      entityModels(parent) :+= model
    } else {
      alwaysVisibleModels += model
    }

    model
  }

  /**
    * List all the visible models.
    */
  def collectVisibleModels(visibleEntities: ArrayBuffer[Entity]): Unit = {
    visibleModels.clear()
    visibleModels ++= alwaysVisibleModels

    {
      var ix = 0
      var num = visibleEntities.length
      while (ix < num) {
        visibleModels ++= entityModels(visibleEntities(ix))
        ix += 1
      }
    }
  }

  /**
    * Calculate new transformation matrices for all the models.
    */
  def updateMatrices(): Unit = {
    for (modelRef <- visibleModels) {
      val parent = modelRef.parent
      val model = modelRef.model
      val state = modelRef.state

      val world = if (parent != null) {
        Matrix43.translate(parent.position) * modelRef.transform
      } else {
        modelRef.transform
      }

      state.worldTransform = world

      for (nodeRef <- modelRef.nodeRefs) {
        state.nodeLocalMatrix(nodeRef.index) = nodeRef.localTransform
      }

      state.updateMatrices()
    }
  }

  private val meshInstances = mutable.HashMap[Mesh, MeshInstance]()
  private val instancedMeshDraws = mutable.ArrayBuffer[InstancedMeshDraw]()

  /**
    * Collect meshes from the models.
    *
    * Must be called after `updateMatrices()`
    */
  def collectMeshInstances(): Unit = {

    meshInstances.clear()
    for (modelRef <- visibleModels) {
      val model = modelRef.model
      val state = modelRef.state

      if (!modelRef.useManualDraws) {
        var ix = 0
        while (ix < model.numMeshes) {
          val mesh = model.meshes(ix)
          val nodeIx = model.meshParentNode(ix)

          val prev = meshInstances.getOrElse(mesh, null)

          // @Todo: Pool MeshInstances?
          val instance = new MeshInstance()
          instance.worldTransform = state.nodeWorldTransform(nodeIx)
          instance.lightProbe = modelRef.lightProbe
          instance.next = prev

          meshInstances(mesh) = instance
          ix += 1
        }
      } else {
        var ix = 0
        if (modelRef.manualDrawList == null)
          modelRef.manualDrawList = new ArrayBuffer[ManualMeshDraw]()
        else
          modelRef.manualDrawList.clear()

        while (ix < model.numMeshes) {
          val mesh = model.meshes(ix)
          val nodeIx = model.meshParentNode(ix)

          val draw = new ManualMeshDraw()
          draw.mesh = mesh
          draw.worldTransform = state.nodeWorldTransform(nodeIx)
          modelRef.manualDrawList += draw

          ix += 1
        }
      }
    }

  }

  /**
    * Update the world uniforms for objects
    *
    * Must be called after `collectMeshInstances()`
    */
  def setupUniforms(): Unit = {
    val renderer = Renderer.get
    val instanceBuffer = new ArrayBuffer[MeshInstance]()
    instancedMeshDraws.clear()

    val lightProbesToUpload = new ArrayBuffer[mutable.ArrayBuffer[LightProbe]]()
    val lightProbeDrawCount = new ArrayBuffer[Int]()

    var currentLightProbes = mutable.ArrayBuffer[LightProbe]()
    var currentLightProbeMap = mutable.HashMap[LightProbe, Int]()
    var currentNumDraws = 0

    for ((mesh, firstInstance) <- meshInstances) {
      var instance = firstInstance

      do {
        instanceBuffer += instance
        instance = instance.next
      } while (instance != null)

      var base = 0
      while (base < instanceBuffer.length) {
        val toDraw = math.min(MaxInstancesPerDraw, instanceBuffer.length - base)

        if (currentLightProbes.size + toDraw > LightProbeUniform.MaxProbes) {
          lightProbesToUpload += currentLightProbes
          lightProbeDrawCount += currentNumDraws

          currentLightProbeMap.clear()
          currentLightProbes = mutable.ArrayBuffer[LightProbe]()
        }

        val draw = new InstancedMeshDraw()
        draw.mesh = mesh
        draw.num = toDraw
        draw.instanceUbo = renderer.pushUniformRef(InstancedUniform, b => {
          var ix = 0
          while (ix < toDraw) {
            val inst = instanceBuffer(base + ix)

            val probeIndex = currentLightProbeMap.getOrElseUpdate(inst.lightProbe, {
              currentLightProbes += inst.lightProbe
              currentLightProbes.length - 1
            })

            val probeOffset = probeIndex * LightProbe.SizeInVec4

            InstancedUniform.World.set(b, ix, inst.worldTransform)
            InstancedUniform.LightInfo.set(b, ix, probeOffset, 0, 0, 0)
            ix += 1
          }
        })
        instancedMeshDraws += draw
        currentNumDraws += 1

        base += toDraw
      }

      instanceBuffer.clear()
    }

    lightProbesToUpload += currentLightProbes
    lightProbeDrawCount += currentNumDraws

    var drawIndex = 0
    for ((probes, numDraws) <- (lightProbesToUpload zip lightProbeDrawCount)) {
      val ubo = renderer.pushUniformRef(LightProbeUniform, u => {
        var ix = 0
        var base = LightProbeUniform.LightProbes.offsetInBytes
        val stride = LightProbeUniform.LightProbes.arrayStrideInBytes
        val baseStride = LightProbe.SizeInVec4 * stride
        while (ix < probes.length) {
          probes(ix).writeToUniform(u, base, stride)
          ix += 1
          base += baseStride
        }
      })

      while (drawIndex < numDraws) {
        instancedMeshDraws(drawIndex).lightProbeUbo = ubo
        drawIndex += 1
      }
    }
  }

  /**
    * Returns a list of draw-calls for instanced meshes for this frame.
    *
    * Must be called after `setupUniforms()`
    */
  def getInstancedMeshDraws(): Seq[InstancedMeshDraw] = instancedMeshDraws

}

