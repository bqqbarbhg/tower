package game

import task.Task

import scala.collection.mutable.ArrayBuffer

package object system {

  var AudioSystem: AudioSystem = null
  var CableRenderSystem: CableRenderSystem = null
  var GroundSystem: GroundSystem = null

  def deferredLoad(): Task[Unit] = {
    val stopAudioTask = for (as <- Option(AudioSystem)) yield {
      Task.Io.add(() => {
        as.joinAudioThread()
      })
    }

    val tasks = new ArrayBuffer[Task[Unit]]()

    tasks += Task.Main.add(stopAudioTask, (_: Option[Unit]) => {
      AudioSystem = new AudioSystem()
    })

    tasks += Task.Main.add(() => {
      CableRenderSystem = new CableRenderSystem()
    })

    Task.Main.add(tasks, (_: Seq[Unit]) => ())
  }

  def unload(): Unit = {
    CableRenderSystem.unload()
    AudioSystem.unload()

    CableRenderSystem = null
  }

}
