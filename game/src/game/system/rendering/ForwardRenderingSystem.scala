package game.system.rendering

import collection.mutable
import collection.mutable.ArrayBuffer

import core._
import render._
import gfx._
import game.shader._
import game.lighting.LightProbe
import game.system.rendering.ForwardRenderingSystem._
import game.system.rendering.ModelSystem._
import render.Renderer.UniformRef

object ForwardRenderingSystem {

  class SkinnedDraw {
    /** Mesh to use */
    var mesh: MeshPart = null

    /** Refernce to `SkinnedModelUniform` */
    var uniform: UniformRef = null
  }

  class InstancedDraw {
    /** Mesh to use */
    var mesh: MeshPart = null

    /** Number of instances to draw */
    var num: Int = 0

    /** Reference to `ModelInstanceUniform` */
    var instanceUbo: UniformRef = null

    /** Reference to `LightProbeUniform` */
    var lightProbeUbo: UniformRef = null
  }

  class Draws {
    val instanced = new ArrayBuffer[InstancedDraw]()
    val skinned = new ArrayBuffer[SkinnedDraw]()
  }

}

trait ForwardRenderingSystem {

  /**
    * Create draw-calls from a map of mesh instances.
    *
    * Note: Must be called on the main thread!
    */
  def createMeshDraws(meshes: MeshInstanceCollection): Draws

}

class ForwardRenderingSystemImpl extends ForwardRenderingSystem {

  override def createMeshDraws(meshes: MeshInstanceCollection): Draws = {
    val res = new Draws()
    val renderer = Renderer.get

    // -- Use instancing to render normal meshes
    // Create instance batches to draw while building a list of light probes in parallel.
    // The light probes are uploaded afterwards and their uniforms are patched to the
    // instanced draws using `lightProbeDrawCount`.

    val lightProbesToUpload = new ArrayBuffer[mutable.ArrayBuffer[LightProbe]]()
    val lightProbeDrawCount = new ArrayBuffer[Int]()

    var currentLightProbes = mutable.ArrayBuffer[LightProbe]()
    var currentLightProbeMap = mutable.HashMap[LightProbe, Int]()
    var currentNumDraws = 0

    for ((mesh, instances) <- meshes.meshes) {

      var base = 0
      while (base < instances.length) {
        val toDraw = math.min(ModelInstanceUniform.MaxInstancesPerDraw, instances.length - base)

        if (currentLightProbes.size + toDraw > LightProbeUniform.MaxProbes) {
          lightProbesToUpload += currentLightProbes
          lightProbeDrawCount += currentNumDraws

          currentLightProbeMap.clear()
          currentLightProbes = mutable.ArrayBuffer[LightProbe]()
        }

        val draw = new InstancedDraw()
        draw.mesh = mesh
        draw.num = toDraw
        draw.instanceUbo = renderer.pushUniformRef(ModelInstanceUniform, b => {
          var ix = 0
          while (ix < toDraw) {
            val inst = instances(base + ix)

            val probeIndex = currentLightProbeMap.getOrElseUpdate(inst.lightProbe, {
              currentLightProbes += inst.lightProbe
              currentLightProbes.length - 1
            })

            ModelInstanceUniform.World.set(b, ix, inst.worldTransform)
            ModelInstanceUniform.LightInfo.set(b, ix, probeIndex, 0, 0, 0)
            ix += 1
          }
        })
        res.instanced += draw
        currentNumDraws += 1

        base += toDraw
      }

    }

    if (currentNumDraws > 0) {
      lightProbesToUpload += currentLightProbes
      lightProbeDrawCount += currentNumDraws
    }

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
        res.instanced(drawIndex).lightProbeUbo = ubo
        drawIndex += 1
      }
    }

    // -- Create individual draws for skinned meshes

    for {
      (mesh, instances) <- meshes.skinnedMeshes
      instance <- instances
    } {
      val draw = new SkinnedDraw()
      draw.mesh = mesh
      draw.uniform = renderer.pushUniformRef(SkinnedModelUniform, u => {
        var ix = 0
        val numBones = instance.bones.length
        while (ix < numBones) {
          SkinnedModelUniform.Bones.set(u, ix, instance.bones(ix))
          ix += 1
        }

        SkinnedModelUniform.writeLightProbe(u, instance.lightProbe)
      })

      res.skinned += draw
    }

    res
  }

}
