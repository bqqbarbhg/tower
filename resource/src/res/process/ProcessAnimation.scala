package res.process

import res.intermediate._

object ProcessAnimation {

  /**
    * Top-level animation processing (in-place).
    */
  def processAnimation(anim: Animation, config: Config.Res.Animation): Unit = {
    AnimationCleanup.flipQuaternions(anim)
    anim.timelines = for (timeline <- anim.timelines) yield {
      var tl = timeline
      if (config.rotationMaxError > 0.0)
        tl = ReduceAnimationKeyframes.reduceRotationKeyframes(tl, config.rotationMaxError)
      if (config.positionMaxError > 0.0)
        tl = ReduceAnimationKeyframes.reducePositionKeyframes(tl, config.positionMaxError)
      if (config.scaleMaxError > 0.0)
        tl = ReduceAnimationKeyframes.reduceSizeKeyframes(tl, config.scaleMaxError)
      tl
    }
    AnimationCleanup.flipQuaternions(anim)
  }

}

