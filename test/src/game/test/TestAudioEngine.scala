package game.test

import audio._
import core._
import audio.output.AudioOutput
import platform.Intrinsic

class TestAudioEngine(val audioOutput: AudioOutput, val renderAudio: (Array[Float], Int) => Unit) {
  @volatile var closeAudio: Boolean = false

  val audioThread = new Thread {
    override def run(): Unit = {
      // Disable denormals for audio performance
      Intrinsic.disableDenormals()

      core.StackAllocator.createCurrentThread(2 * 1024 * 1024)
      audioOutput.open()

      val MaxAudioLatency = 44100 / 10
      var framesWritten: Long = 0

      val BeginSilenceLength = 1500
      val EndSilenceLength = 1500
      val FadeInLength = 500
      val FadeOutLength = 500

      val ChunkSize = 1000
      val chunk = new Array[Float](ChunkSize * 2)

      {
        java.util.Arrays.fill(chunk, 0.0f)
        var silenceWritten = 0
        while (silenceWritten < BeginSilenceLength) {
          val toWrite = math.min(BeginSilenceLength - silenceWritten, ChunkSize)
          framesWritten += toWrite
          audioOutput.write(chunk, toWrite)
          silenceWritten += toWrite
        }
      }

      renderAudio(chunk, FadeInLength)
      framesWritten += FadeInLength
      for (i <- 0 until FadeInLength) {
        val s = i * 2
        val fade = i.toFloat / FadeInLength.toFloat
        chunk(s + 0) *= fade
        chunk(s + 1) *= fade
      }
      audioOutput.write(chunk, FadeInLength)

      audioOutput.start()

      while (!closeAudio) {
        val playPos = audioOutput.playbackFrame
        val lead = framesWritten - playPos
        val maxWrite = math.max(MaxAudioLatency - lead, 0)
        if (maxWrite > 0) {
          val toWrite = math.min(maxWrite, ChunkSize).toInt
          renderAudio(chunk, toWrite)
          framesWritten += toWrite
          audioOutput.write(chunk, toWrite)
        } else {
          Thread.sleep(5)
        }
      }

      renderAudio(chunk, FadeOutLength)
      framesWritten += FadeOutLength
      for (i <- 0 until FadeOutLength) {
        val s = i * 2
        val fade = 1.0f - i.toFloat / FadeOutLength.toFloat
        chunk(s + 0) *= fade
        chunk(s + 1) *= fade
      }
      audioOutput.write(chunk, FadeOutLength)

      {
        java.util.Arrays.fill(chunk, 0.0f)
        var silenceWritten = 0
        while (silenceWritten < EndSilenceLength) {
          val toWrite = math.min(EndSilenceLength - silenceWritten, ChunkSize)
          framesWritten += toWrite
          audioOutput.write(chunk, toWrite)
          silenceWritten += toWrite
        }
      }

      println("Closing audio")
      audioOutput.close()
    }
  }
  audioThread.start()
}

