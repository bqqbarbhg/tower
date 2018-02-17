package res.runner

import java.nio.ByteBuffer
import java.io.File

import core._
import util.BufferUtils._

/**
  * A file containing caching information about an output file.
  */
class OutputCacheFile {
  var outputHash: Long = 0
  var outputSize: Long = 0

  def write(buf: ByteBuffer): Unit = {
    val Version = 1
    buf.putMagic("s2oc")
    buf.putVersion(Version)
    buf.putLong(outputHash)
    buf.putLong(outputSize)
    buf.putMagic("E.oc")
  }

  def read(buf: ByteBuffer): Unit = {
    buf.verifyMagic("s2oc")
    val MaxVersion = 1
    val version = buf.getVersion(MaxVersion)
    outputHash = buf.getLong()
    outputSize = buf.getLong()
    buf.verifyMagic("E.oc")
  }

  /** Load a cache file */
  def load(file: File): Unit = withStack {
    val buf = alloca(1024)
    buf.readFromFile(file)
    buf.finish()
    this.read(buf)
  }

  /** Save a cache file */
  def save(file: File): Unit = withStack {
    val buf = alloca(1024)
    this.write(buf)
    buf.finish()
    buf.writeToFile(file)
  }
}

