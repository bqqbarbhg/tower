package game.system.rendering

import collection.mutable
import collection.mutable.ArrayBuffer

import core._
import render._
import gfx._
import game.shader._
import game.system.rendering.ModelSystem._
import render.opengl.RendererGl.UniformRef
import render.Renderer.UniformRef
import ShadowRenderingSystem._

object ShadowRenderingSystem {

  class SkinnedDraw {
    /** Mesh to use */
    var mesh: MeshPart = null

    /** Refernce to `SkinnedShadowUniform` */
    var uniform: UniformRef = null
  }

  class InstancedDraw {
    /** Mesh to use */
    var mesh: MeshPart = null

    /** Number of instances to draw */
    var num: Int = 0

    /** Reference to `ShadowInstanceUniform` */
    var instanceUbo: UniformRef = null
  }

  class Draws {
    val instanced = new ArrayBuffer[InstancedDraw]()
    val skinned = new ArrayBuffer[SkinnedDraw]()
  }

}

trait ShadowRenderingSystem {

  /**
    * Create draw-calls from a map of mesh instances.
    *
    * Note: Must be called on the main thread!
    */
  def createMeshDraws(meshes: MeshInstanceCollection): Draws

}

class ShadowRenderingSystemImpl extends ShadowRenderingSystem {

  override def createMeshDraws(meshes: MeshInstanceCollection) = {
    val res = new Draws()
    val renderer = Renderer.get

    for ((mesh, instances) <- meshes.meshes) {

      var base = 0
      while (base < instances.length) {
        val toDraw = math.min(ShadowInstanceUniform.MaxInstancesPerDraw, instances.length - base)

        val draw = new InstancedDraw()
        draw.mesh = mesh
        draw.num = toDraw
        draw.instanceUbo = renderer.pushUniformRef(ShadowInstanceUniform, b => {
          var ix = 0
          while (ix < toDraw) {
            val inst = instances(base + ix)
            ShadowInstanceUniform.World.set(b, ix, inst.worldTransform)
            ix += 1
          }
        })
        res.instanced += draw
        base += toDraw
      }

    }

    for {
      (mesh, instances) <- meshes.skinnedMeshes
      instance <- instances
    } {
      val draw = new SkinnedDraw()
      draw.mesh = mesh
      draw.uniform = renderer.pushUniformRef(SkinnedShadowUniform, u => {
        var ix = 0
        val numBones = instance.bones.length
        while (ix < numBones) {
          SkinnedShadowUniform.Bones.set(u, ix, instance.bones(ix))
          ix += 1
        }
      })

      res.skinned += draw
    }

    res
  }

}

