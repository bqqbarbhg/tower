package tower.engine.audio.source

import tower.engine.audio.{SampleSource, SampleCursor}

abstract class PcmCursor extends SampleCursor {
  var pos: Int = 0
  def seek(sampleIndex: Int): Unit = { pos = sampleIndex }
  def close(): Unit = { }
}

class PcmCursorMono(source: PcmSource) extends PcmCursor {
  def read(samples: Array[Float], offset: Int, num: Int): Unit = {
    var i = 0
    val data = source.data
    val base = pos
    while (i < num) {
      val ii = i << 1
      val s = data(base + i).toFloat * 0.00003051757f
      samples(offset + ii) = s
      samples(offset + ii + 1) = s
      i += 1
    }
    pos += num
  }
}

class PcmCursorStereo(source: PcmSource) extends PcmCursor {
  def read(samples: Array[Float], offset: Int, num: Int): Unit = {
    var i = 0
    val data = source.data
    val base = pos * 2
    val end = num * 2
    while (i < end) {
      samples(offset + i) = data(base + i).toFloat * 0.00003051757f
      samples(offset + i + 1) = data(base + i + 1).toFloat * 0.00003051757f
      i += 2
    }
    pos += num
  }
}

class PcmCursorAny(source: PcmSource, channels: Int) extends PcmCursor {
  def read(samples: Array[Float], offset: Int, num: Int): Unit = {
    var i = 0
    val data = source.data
    while (i < num) {
      val ix = (pos + i) * channels
      samples(offset + i*2) = data(ix).toFloat * 0.00003051757f
      samples(offset + i*2 + 1) = data(ix + 1).toFloat * 0.00003051757f
      i += 1
    }
    pos += num
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
