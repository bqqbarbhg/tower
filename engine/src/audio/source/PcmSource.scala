package audio.source

abstract class PcmCursor extends SampleCursor {
  var pos: Int = 0
  def seek(sampleIndex: Int): Unit = { pos = sampleIndex }
  def close(): Unit = { }
}

class PcmCursorMono(source: PcmSource) extends PcmCursor {
  def read(samples: Array[Float], offsetInFrames: Int, numFrames: Int): Unit = {
    var i = 0
    val data = source.data
    val base = pos
    val dstBase = offsetInFrames * 2
    while (i < numFrames) {
      val ii = i << 1
      val s = data(base + i).toFloat * 0.00003051757f
      samples(dstBase + ii) = s
      samples(dstBase + ii + 1) = s
      i += 1
    }
    pos += numFrames
  }
}

class PcmCursorStereo(source: PcmSource) extends PcmCursor {
  def read(samples: Array[Float], offsetInFrames: Int, numFrames: Int): Unit = {
    var i = 0
    val data = source.data
    val base = pos * 2
    val end = numFrames * 2
    val dstBase = offsetInFrames * 2
    while (i < end) {
      samples(dstBase + i) = data(base + i).toFloat * 0.00003051757f
      samples(dstBase + i + 1) = data(base + i + 1).toFloat * 0.00003051757f
      i += 2
    }
    pos += numFrames
  }
}

class PcmCursorAny(source: PcmSource, channels: Int) extends PcmCursor {
  def read(samples: Array[Float], offsetInFrames: Int, numFrames: Int): Unit = {
    var i = 0
    val data = source.data
    val dstBase = offsetInFrames * 2
    while (i < numFrames) {
      val ix = (pos + i) * channels
      samples(dstBase + i*2) = data(ix).toFloat * 0.00003051757f
      samples(dstBase + i*2 + 1) = data(ix + 1).toFloat * 0.00003051757f
      i += 1
    }
    pos += numFrames
  }
}

class PcmSource(val data: Array[Short], val channels: Int) extends SampleSource {
  override def open(): SampleCursor = {
    if (channels == 1) {
      new PcmCursorMono(this)
    } else if (channels == 2) {
      new PcmCursorStereo(this)
    } else {
      new PcmCursorAny(this, channels)
    }
  }
}
