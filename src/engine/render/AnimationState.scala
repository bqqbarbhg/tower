package tower.engine.render

import tower.math._
import AnimationState._

import scala.collection.mutable.ArrayBuffer

object AnimationState {

  class AnimationLayer(val animation: Animation, val model: Model) {

    private val timelineToNode = model.resolveAnimationTimelineNodeIndices(animation)
    private val animationState = Array.fill(animation.timelines.length * Animation.Timeline.StateSize)(0)

    var time: Double = 0.0
    var alpha: Double = 1.0

    def apply(state: AnimationState): Unit = {
      var ix = 0
      val numTimelines = animation.timelines.length

      while (ix < numTimelines) {
        val timeline = animation.timelines(ix)
        val node = timelineToNode(ix)
        val frame = timeline.evaluate(time, animationState, ix * Animation.Timeline.StateSize)

        if (alpha < 0.99) {
          val clampedAlpha = if (alpha >= 0.0) alpha else 0.0
          val prev = state.composition(node)
          state.composition(node) = Animation.Frame.lerp(prev, frame, clampedAlpha)
        } else {
          state.composition(node) = frame
        }

        ix += 1
      }

    }

  }

}

class AnimationState(val model: Model) {

  val transform = Array.fill(model.numNodes)(Matrix4.makeIdentity)
  val layers = new ArrayBuffer[AnimationLayer]()
  var worldTransform = Matrix4.Identity

  private var tmpMatrix = new Matrix4()

  // Temporary, contains valid frames only during `apply`. Otherwise identity.
  private val composition = Array.fill(model.numNodes)(Animation.Frame.Identity)

  def apply(): Unit = {
    var ix = 0
    val numTransform = transform.length

    // 1. Apply animation layers
    for (layer <- layers) {
      layer.apply(this)
    }

    // 2. Apply transforms
    transform(0).unsafeMulRight(worldTransform)
    ix = 1
    while (ix < numTransform) {
      val parentIx = model.parentIndex(ix)
      transform(ix).unsafeMul(transform(parentIx), model.transformToParent(ix))

      val frame = composition(ix)
      Matrix4.unsafeWorldRot(tmpMatrix, frame.rotation, frame.scale, frame.position)
      transform(ix).unsafeMulRight(tmpMatrix)

      // Don't hang on to garbage
      composition(ix) = Animation.Frame.Identity
      ix += 1
    }
  }

  def addAnimation(anim: Animation): AnimationLayer = {
    val layer = new AnimationLayer(anim, model)
    layers += layer
    layer
  }

}
