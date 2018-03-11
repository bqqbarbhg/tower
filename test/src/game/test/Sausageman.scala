package game.test

import asset.ModelAsset
import core._
import scene.AnimationController
import Sausageman._
import gfx.{Model, ModelState}

object Sausageman {

  val AnimWalk = Identifier("Walk")
  val AnimRun = Identifier("Run")
  val AnimPunch = Identifier("Punch")


}

class Sausageman(val modelAsset: ModelAsset) {
  var speed: Double = 0.0

  private var model: Model = null
  private var animatorImpl: Animator = null

  class Animator(val model: Model) {
    val modelState = new ModelState(model)
    val controller = new AnimationController(model)

    val walkAnim = controller.addManual(AnimWalk, 0)
    val runAnim = controller.addManual(AnimRun, 1)

    def doPunch(): Unit = {
      val p = controller.play(AnimPunch, 2)
      p.fadeInDuration = 0.3
      p.fadeOutDuration = 0.3
      p.playbackSpeed = 1.5
    }

    def update(dt: Double): Unit = {
      walkAnim.advanceLoop(speed * dt)
      runAnim.time = walkAnim.time

      runAnim.alpha = clamp((speed - 0.8) / 0.4, 0.0, 1.0)

      controller.update(dt)

      controller.apply(modelState)
      modelState.updateMatrices()
    }
  }

  def animator: Animator = {
    if (model == null || !model.loaded) {
      model = modelAsset.get
      animatorImpl = new Animator(model)
    }

    animatorImpl
  }

  def doPunch(): Unit = {
    animator.doPunch()
  }

  def update(dt: Double): Unit = {
    animator.update(dt)
  }

}
