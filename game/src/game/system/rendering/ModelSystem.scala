package game.system.rendering

import scala.collection.mutable
import core._
import gfx._
import render._
import asset.ModelAsset
import game.system._
import game.system.Entity._
import game.lighting.LightProbe
import game.system.rendering.ModelSystem._
import game.system.rendering.ModelSystemImpl._
import render.Renderer.UniformRef

import scala.collection.mutable.ArrayBuffer

object ModelSystem {

  class MeshInstance {

    /** Orientation of the mesh in the world */
    var worldTransform: Matrix43 = Matrix43.Identity

    /** Potential light probe for the mesh */
    var lightProbe: LightProbe = null

  }

  class SkinnedMeshInstance {

    /** Bone transforms for the mesh */
    var bones: Array[Matrix43] = _

    /** Potential light probe for the mesh */
    var lightProbe: LightProbe = null

  }

  class MeshInstanceCollection {
    val meshes = new mutable.HashMap[MeshPart, ArrayBuffer[MeshInstance]]()
    val skinnedMeshes = new mutable.HashMap[MeshPart, ArrayBuffer[SkinnedMeshInstance]]()
  }

  /**
    * A model instance in the world.
    */
  abstract sealed class ModelInstance {

    /** Transform relative to the entity. */
    var transform: Matrix43 = Matrix43.Identity

    /** Light probe the model uses. */
    var lightProbe: LightProbe = LightProbe.Empty

    /** Find a node matching a name in the model. */
    def findNode(name: Identifier): Option[NodeInstance]

    /** Delete the ModelInstance from the system */
    def delete(): Unit

    /** Current model reference, _not persistent between frames_ */
    var model: Model = _

    /** Current model state reference, _not persistent between frames_ */
    var state: ModelState = _
  }

  /**
    * A node inside a model instance.
    */
  abstract sealed class NodeInstance(val name: Identifier) {

    /** Parent model of this node */
    def model: ModelInstance

    /** Transform applied to the node relative to its entity. */
    var localTransform: Matrix43 = Matrix43.Identity

  }

}

sealed trait ModelSystem extends EntityDeleteListener {

  /**
    * Called when the asset load state has changed.
    * Re-resolves all models from assets.
    */
  def assetsLoaded(): Unit

  /**
    * Shorthand for adding a model to the world with an asset name.
    * See the other overload for more documentation.
    */
  def addModel(entity: Entity, assetName: Identifier): ModelInstance = addModel(entity, ModelAsset(assetName))

  /**
    * Add a model to the world that is parented to `entity`. This means that
    * the root transform of the model will follow the transform of the entity.
    * The model will also be deleted when the entity is deleted.
    *
    * @param entity Parent entity for the model.
    * @param asset Asset to use for the model.
    * @return A model reference that can be used to manipulate the model.
    */
  def addModel(entity: Entity, asset: ModelAsset): ModelInstance

  /**
    * Remove all the models and the entity itself from this system.
    */
  def removeModels(entity: Entity): Unit

  /**
    * Collect models from a set of visible entities.
    */
  def collectVisibleModels(visible: EntitySet): ArrayBuffer[ModelInstance]

  /**
    * Collect models from a set of entities.
    */
  def collectModels(entities: Iterable[Entity]): ArrayBuffer[ModelInstance]

  /**
    * Collect models from an entity.
    */
  def collectModels(entity: Entity): ArrayBuffer[ModelInstance] = collectModels(Some(entity))

  /**
    * Update a set of models. If the same model is passed to this method twice
    * in a frame only the first update is processed.
    */
  def updateModels(models: ArrayBuffer[ModelInstance]): Unit

  /**
    * Collect all the visible meshes from a set of models.
    */
  def collectMeshInstances(models: ArrayBuffer[ModelInstance]): MeshInstanceCollection

  /**
    * Clean-up things after every frame.
    */
  def frameCleanup(): Unit

}

object ModelSystemImpl {

  class ModelInstanceImpl(val entity: Entity, val asset: ModelAsset, val next: ModelInstanceImpl) extends ModelInstance {
    model = asset.get
    state = new ModelState(model)

    var nodes = Array[NodeInstanceImpl]()
    var poolIndex: Int = -1
    var lastFrameUpdated: Long = -1L
    var worldTransform = new Matrix43.Unsafe()
    var entityTransform = new Matrix43.Unsafe()

    override def findNode(name: Identifier): Option[NodeInstance] = {
      if (name == Identifier.Empty) return None

      var index = model.findNodeByName(name)
      if (index >= 0) {
        val ref = new NodeInstanceImpl(this, name, index)
        nodes :+= ref
        Some(ref)
      } else {
        None
      }
    }

    override def delete(): Unit = ???
  }

  class NodeInstanceImpl(override val model: ModelInstanceImpl, name: Identifier, var index: Int) extends NodeInstance(name) {
  }

}

final class ModelSystemImpl extends ModelSystem {

  val entityToModel = new mutable.HashMap[Entity, ModelInstanceImpl]()
  val allModels = new ArrayPool[ModelInstanceImpl]()
  val visibleModels = new ArrayBuffer[ModelInstanceImpl]()
  var currentFrameIndex: Long = 1L

  def updateModel(modelInst: ModelInstanceImpl): Unit = {
    val entity = modelInst.entity
    val model = modelInst.model
    val state = modelInst.state

    if (!entity.static || modelInst.lastFrameUpdated < 0L) {
      Matrix43.unsafeWorld(modelInst.entityTransform, entity.position, entity.rotation)
      modelInst.worldTransform.unsafeMul(modelInst.transform, modelInst.entityTransform)
      state.worldTransform = modelInst.worldTransform
    }

    for (node <- modelInst.nodes) {
      state.nodeLocalMatrix(node.index) = node.localTransform
    }

    state.updateMatrices()
  }

  override def assetsLoaded(): Unit = {
    for (model <- allModels) {
      if (!model.model.loaded) {
        model.model = model.asset.get
        model.state = new ModelState(model.model)
        model.lastFrameUpdated = -1L
        for (ref <- model.nodes) {
          ref.index = model.model.findNodeByName(ref.name)
        }
      }
    }
  }

  override def addModel(entity: Entity, asset: ModelAsset): ModelInstance = {
    val next = if (entity.hasFlag(Flag_Model)) entityToModel(entity) else null
    val model = new ModelInstanceImpl(entity, asset, next)

    model.poolIndex = allModels.add(model)
    entityToModel(entity) = model

    entity.setFlag(Flag_Model)

    model
  }

  override def removeModels(entity: Entity): Unit = {
    var model = entityToModel(entity)
    do {
      allModels.remove(model.poolIndex)
      model = model.next
    } while (model != null)
    entity.clearFlag(Flag_Model)
  }

  override def collectVisibleModels(visible: EntitySet): ArrayBuffer[ModelInstance] = {
    val result = new ArrayBuffer[ModelInstance]()
    for (entity <- visible.flag(Flag_Model)) {
      var model = entityToModel(entity)
      do {
        result += model
        model = model.next
      } while (model != null)
    }
    result
  }

  override def collectModels(entities: Iterable[Entity]): ArrayBuffer[ModelInstance] = {
    val result = new ArrayBuffer[ModelInstance]()
    for (entity <- entities.filter(_.hasFlag(Flag_Model))) {
      var model = entityToModel(entity)
      do {
        result += model
        model = model.next
      } while (model != null)
    }
    result
  }

  override def updateModels(models: ArrayBuffer[ModelInstance]): Unit = {
    var ix = 0
    val len = models.length
    while (ix < len) {
      val model = models(ix).asInstanceOf[ModelInstanceImpl]

      if (model.lastFrameUpdated != currentFrameIndex) {
        updateModel(model)
        model.lastFrameUpdated = currentFrameIndex
      }

      ix += 1
    }
  }

  override def collectMeshInstances(models: ArrayBuffer[ModelInstance]): MeshInstanceCollection = {
    val res = new MeshInstanceCollection()

    var ix = 0
    val len = models.length
    while (ix < len) {
      val modelInst = models(ix).asInstanceOf[ModelInstanceImpl]
      val model = modelInst.model
      val state = modelInst.state

      var meshIx = 0
      val meshLen = model.meshes.length
      while (meshIx < meshLen) {
        val mesh = model.meshes(meshIx)

        var partIx = 0
        while (partIx < mesh.parts.length) {
          val part = mesh.parts(partIx)

          if (part.numBones > 0) {

            val inst = new SkinnedMeshInstance()
            inst.bones = new Array[Matrix43](part.numBones)
            inst.lightProbe = modelInst.lightProbe

            val mapping = model.skinnedPartBoneToNodeMapping(meshIx)(partIx)
            var boneIx = 0
            while (boneIx < part.numBones) {
              val nodeIx = mapping(boneIx)
              inst.bones(boneIx) = state.nodeWorldTransform(nodeIx) * part.boneMeshToBone(boneIx)
              boneIx += 1
            }

            val list = res.skinnedMeshes.getOrElseUpdate(part, new ArrayBuffer[SkinnedMeshInstance]())
            list += inst

          } else {
            val nodeIx = model.meshParentNode(meshIx)

            val inst = new MeshInstance()
            inst.worldTransform = state.nodeWorldTransform(nodeIx)
            inst.lightProbe = modelInst.lightProbe

            val list = res.meshes.getOrElseUpdate(part, new ArrayBuffer[MeshInstance]())
            list += inst
          }

          partIx += 1
        }

        meshIx += 1
      }

      ix += 1
    }

    res
  }

  override def frameCleanup(): Unit = {
    currentFrameIndex += 1L
  }

  override def entitiesDeleted(entities: EntitySet): Unit = entities.flag(Flag_Model).foreach(removeModels)
}


