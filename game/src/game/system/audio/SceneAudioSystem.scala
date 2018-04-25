package game.system.audio

import core._
import asset._
import game.component.SoundInfo
import game.system._
import game.system.audio.AudioSystem._
import SceneAudioSystemImpl._

import scala.util.Random

sealed trait SceneAudioSystem {

  /** Update the sounds in the scene */
  def update(cameraPosition: Vector3, cameraDirection: Vector3): Unit

  /** Add a 3D positional sound */
  def play(position: Vector3, soundInfo: SoundInfo): Unit

  /** Stop all the playing sounds */
  def unload(): Unit

}

object SceneAudioSystemImpl {

  class SceneSoundInstance(val position: Vector3, val ref: SoundRef, val info: SoundInfo, val baseVolume: Double) extends CompactArrayPool.Element {
  }

}

final class SceneAudioSystemImpl extends SceneAudioSystem {

  val random = new Random()
  var activeSounds = new CompactArrayPool[SceneSoundInstance]()
  var cameraPosition: Vector3 = Vector3.Zero
  var cameraDirection: Vector3 = Vector3.Zero
  var cameraRight: Vector3 = Vector3.Zero

  def updateSoundAttenuation(sound: SceneSoundInstance): Unit = {
    val dir = sound.position - cameraPosition

    val dist = dir.length / sound.info.attenuation
    val dirPan = Vector2(dir dot cameraRight, dir dot cameraDirection).normalizeOrZero

    sound.ref.instance.pan = dirPan.x
    sound.ref.instance.volume = sound.baseVolume / (1.0 + dist*dist)
  }

  override def play(position: Vector3, soundInfo: SoundInfo): Unit = {
    if (soundInfo.assets.isEmpty) return

    val pitch = if (soundInfo.pitchVariation >= 0.001)
      soundInfo.pitch + soundInfo.pitchVariation * (random.nextDouble() - 0.5) * 2.0
    else
      soundInfo.pitch

    val index = random.nextInt(soundInfo.assets.length)
    val asset = soundInfo.assets(index)
    val chan = soundInfo.audioChannel
    val ref = audioSystem.play(asset, chan, soundInfo.volume, 0.0, pitch)
    val inst = new SceneSoundInstance(position, ref, soundInfo, ref.instance.volume)
    updateSoundAttenuation(inst)
  }

  override def update(cameraPosition: Vector3, cameraDirection: Vector3): Unit = {

    this.cameraPosition = cameraPosition
    this.cameraDirection = cameraDirection
    this.cameraRight = (cameraDirection cross Vector3.Up)

    var ix = 0
    while (ix < activeSounds.length) {

      val sound = activeSounds(ix)
      if (sound.ref.instance.ended) {
        activeSounds.remove(ix)
      } else {
        updateSoundAttenuation(sound)
      }

      ix += 1
    }

  }

  override def unload(): Unit = {
    for (sound <- activeSounds) {
      sound.ref.stop()
    }
    activeSounds.clear()
  }

}

