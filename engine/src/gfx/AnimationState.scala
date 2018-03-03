package gfx

import core._

class AnimationState(val model: Model, val animation: Animation) {

  /** Mapping of animation timeline indices to model node indices. */
  private var timelineToNode = model.resolveAnimationTimelineNodeIndices(animation)

  /** `Animation` needs some state */
  private var opaqueState = animation.createState()

  /** Current time in seconds */
  var time: Double = 0.0

  /** Current blend factor [0.0, 1.0] */
  var alpha: Double = 1.0

  /**
    * Apply the animation at the current time.
    */
  def apply(state: ModelState): Unit = {
    // Don't apply hidden animations
    if (alpha <= 0.0) return

    var ix = 0
    val numTimelines = animation.timelines.length

    while (ix < numTimelines) {
      val node = timelineToNode(ix)
      if (node >= 0) {
        val timeline = animation.timelines(ix)
        val frame = timeline.evaluate(time, opaqueState)

        if (alpha < 0.999) {
          val prev = state.nodeTransform(node)
          state.nodeTransform(node) = AffineTransform.lerp(prev, frame, alpha)
        } else {
          state.nodeTransform(node) = frame
        }
      }
      ix += 1
    }

  }

}

