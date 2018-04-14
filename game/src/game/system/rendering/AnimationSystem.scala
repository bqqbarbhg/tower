package game.system.rendering

import core._
import game.system._
import game.system.Entity._
import game.system.rendering.ModelSystem.ModelInstance
import AnimationSystem._
import AnimationSystemImpl._
import gfx.{Animation, AnimationState, ModelState}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object AnimationSystem {

  sealed abstract class AnimationChannel(val layer: Int) {
    var speed: Double = 1.0
    var alpha: Double = 1.0

    def animation: Animation

    def time: Double
    def time_=(v: Double): Unit

    /** Abruptly remove the animation from the animator */
    def stop(): Unit
  }

  sealed abstract class Animator(val model: ModelInstance) {

    /** Play an animation once and remove it after it's done */
    def playOnce(layer: Int, animation: Identifier, fadeIn: Double, fadeOut: Double): AnimationChannel

    /** Play an animation with an infinite loop */
    def playLoop(layer: Int, animation: Identifier): AnimationChannel

  }

}

sealed trait AnimationSystem extends EntityDeleteListener {

  /**
    * Called when the asset load state has changed.
    * Re-resolves AnimationStates from assets.
    *
    * Must be called after ModelSystem.assetsLoaded()!
    */
  def assetsLoaded(): Unit

  /** Add or get an existing animation controller for an entity */
  def addAnimator(entity: Entity, model: ModelInstance): Animator

  /** Update all visible animations */
  def updateVisibleAnimations(dt: Double, visible: EntitySet): Unit

}

object AnimationSystemImpl {

  final class AnimationChannelImpl(layer: Int, val serial: Int, var state: AnimationState, val fadeIn: Double, val fadeOut: Double, val loop: Boolean) extends AnimationChannel(layer) {
    val sortKey: Long = (layer.toLong << 32L) | (serial.toLong & 0xFFFFFFFFL)

    var done: Boolean = false

    override def stop(): Unit = done = true

    override def animation: Animation = state.animation

    override def time: Double = state.time
    override def time_=(v: Double): Unit = state.time = v

    def update(dt: Double, modelState: ModelState): Boolean = {
      if (done) return true

      val time = state.time
      val fIn = if (fadeIn > 0.001) math.min(time / fadeIn, 1.0) else 1.0
      val fOut = if (fadeOut > 0.001) math.min((state.animation.duration - time) / fadeOut, 1.0) else 1.0
      state.alpha = alpha * fIn * fOut

      state.apply(modelState)

      if (loop) {
        state.advanceLoop(dt * speed)
      } else {
       done = state.advanceClamp(dt * speed)
      }

      done
    }

  }

  final class AnimatorImpl(model: ModelInstance, val next: AnimatorImpl) extends Animator(model) {
    var channels = new Array[AnimationChannelImpl](0)
    var lastUpdatedTime: Double = 0.0

    def add(channel: AnimationChannelImpl): Unit = {
      channels = (channels :+ channel).sortBy(_.sortKey)
    }

    def update(time: Double): Unit = {
      val dt = time - lastUpdatedTime
      lastUpdatedTime = time

      val modelState = model.state
      var anyDone = false
      for (channel <- channels) {
        if (channel.update(dt, modelState))
          anyDone = true
      }
      if (anyDone)
        channels = channels.filterNot(_.done)
    }

    def reload(): Unit = {
      for (c <- channels) {
        c.state = c.state.remake(model.model)
      }
    }

    override def playOnce(layer: Int, animation: Identifier, fadeIn: Double, fadeOut: Double): AnimationChannel = {
      val system = animationSystem.asInstanceOf[AnimationSystemImpl]
      val serial = system.nextSerial()
      val state = new AnimationState(model.model, model.model.findAnimationByName(animation))
      val channel = new AnimationChannelImpl(layer, serial, state, fadeIn, fadeOut, false)
      add(channel)
      channel
    }

    override def playLoop(layer: Int, animation: Identifier): AnimationChannel = {
      val system = animationSystem.asInstanceOf[AnimationSystemImpl]
      val serial = system.nextSerial()
      val state = new AnimationState(model.model, model.model.findAnimationByName(animation))
      val channel = new AnimationChannelImpl(layer, serial, state, 0.0, 0.0, true)
      add(channel)
      channel
    }
  }

}

final class AnimationSystemImpl extends AnimationSystem {

  val entityToAnimated = new mutable.HashMap[Entity, AnimatorImpl]()
  var currentTime: Double = 0.0
  var currentAnimationSerial: Int = 0

  def nextSerial(): Int = {
    currentAnimationSerial += 1
    currentAnimationSerial
  }

  override def assetsLoaded(): Unit = {
    for (firstAnim <- entityToAnimated.valuesIterator) {
      var anim = firstAnim
      do {
        anim.reload()
        anim = anim.next
      } while (anim != null)
    }
  }

  override def addAnimator(entity: Entity, model: ModelInstance): Animator = {
    val next = if (entity.hasFlag(Flag_Animator)) entityToAnimated(entity) else null

    if (next != null) {
      var search = next
      do {
        if (search.model == model) return search
        search = search.next
      } while (search != null)
    }

    val animator = new AnimatorImpl(model, next)
    animator.lastUpdatedTime = currentTime

    entityToAnimated(entity) = animator
    entity.setFlag(Flag_Animator)

    animator
  }

  override def updateVisibleAnimations(dt: Double, visible: EntitySet): Unit = {
    currentTime += dt

    for (e <- visible.flag(Flag_Animator)) {
      var anim = entityToAnimated(e)
      do {
        anim.update(currentTime)
        anim = anim.next
      } while (anim != null)
    }
  }

  override def entitiesDeleted(entities: EntitySet): Unit = {
    for (e <- entities.flag(Flag_Animator)) {
      var animator = entityToAnimated.remove(e).get
      do {
        animator = animator.next
      } while (animator != null)
      e.clearFlag(Flag_Animator)
    }
  }
}

