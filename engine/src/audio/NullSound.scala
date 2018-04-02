package audio

import audio.source.NullSource
import core.Identifier

object NullSound extends Sound(Identifier("<NULL>")) {
  format = "NULL"
  numChannels = 2
  sampleRate = 44100
  lengthInFrames = 1024
  sampleSource = NullSource
  refCount += 1
}


