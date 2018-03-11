package scene

import core._
import gfx._

import collection.mutable.ArrayBuffer
import util.BinarySearch

import AnimationController._

object AnimationController {

  class AnimationPlayer(val state: AnimationState) {
    var fadeInDuration: Double = 0.0
    var fadeOutDuration: Double = 0.0
    var playbackSpeed: Double = 1.0
    var alpha: Double = 1.0
  }

}

class AnimationController(val model: Model) {
  val orders = new ArrayBuffer[Int]()
  val states = new ArrayBuffer[AnimationState]()
  val players = new ArrayBuffer[AnimationPlayer]()
  val playersToRemove = new ArrayBuffer[AnimationPlayer]()

  def addManual(name: Identifier, order: Int): AnimationState = {
    val anim = model.findAnimationByName(name)
    val as = new AnimationState(model, anim)
    val ix = BinarySearch.upperBound(0, orders.length, i => orders(i) > order)
    orders.insert(ix, order)
    states.insert(ix, as)
    as
  }

  def play(name: Identifier, order: Int): AnimationPlayer = {
    val as = addManual(name, order)
    val player = new AnimationPlayer(as)
    players += player
    player
  }

  def remove(as: AnimationState): Unit = {
    val ix = states.indexOf(as)
    orders.remove(ix)
    states.remove(ix)
  }

  def update(dt: Double): Unit = {
    for (player <- players) {
      player.state.advanceClamp(dt * player.playbackSpeed)
      val time = player.state.time
      val duration = player.state.animation.duration

      if (player.state.time >= player.state.animation.duration - 0.0001) {
        playersToRemove += player
      } else {
        val fadeIn = if (player.fadeInDuration > 0.001) math.min(time / player.fadeInDuration, 1.0) else 1.0
        val fadeOut = if (player.fadeInDuration > 0.001) math.min((duration - time) / player.fadeOutDuration, 1.0) else 1.0
        player.state.alpha = fadeIn * fadeOut * player.alpha
      }
    }

    for (player <- playersToRemove) {
      remove(player.state)
      players -= player
    }
    playersToRemove.clear()
  }

  def apply(modelState: ModelState): Unit = {
    for (state <- states) {
      state.apply(modelState)
    }
  }
}

