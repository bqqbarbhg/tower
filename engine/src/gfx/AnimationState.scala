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

  /** Is the content required by the animation state loaded */
  def loaded: Boolean = model.loaded && animation.loaded

  /** Copy the AnimationState but update references to newly loaded assets */
  def remake(newModel: Model): AnimationState = {
    val newAnimation = newModel.findAnimationByName(animation.name)
    val copy = new AnimationState(newModel, newAnimation)
    copy.time = time
    copy.alpha = alpha
    copy
  }

  /**
    * Advance the animation by some timestep, but loop when the animation reaches the end.
    */
  def advanceLoop(dt: Double): Unit = {
    time += dt
    time %= animation.duration
  }

  /**
    * Advance the animation by some timestep, but clamp to the end.
    */
  def advanceClamp(dt: Double): Boolean = {
    time += dt
    if (time >= animation.duration) {
      time = animation.duration
      true
    } else {
      false
    }
  }

  /**
    * Apply the animation at the current time.
    */
  def apply(state: ModelState): Unit = {
    // Don't apply hidden animations
    if (alpha <= 0.0) return

    // Don't apply stale animations
    if (!loaded) return

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

