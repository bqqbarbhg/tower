package res.runner

import java.nio.ByteBuffer
import java.io.File

import core._
import util.BufferUtils._

/**
  * A file containing caching information about an asset source file.
  */
class AssetCacheFile {
  var configFormatHash: Long = 0
  var configHash: Long = 0
  var sourceHash: Long = 0
  var sourceTimestamp: Long = 0

  def write(buf: ByteBuffer): Unit = {
    val Version = 1
    buf.putMagic("s2ac")
    buf.putVersion(Version)
    buf.putLong(configFormatHash)
    buf.putLong(configHash)
    buf.putLong(sourceHash)
    buf.putLong(sourceTimestamp)
    buf.putMagic("E.ac")
  }

  def read(buf: ByteBuffer): Unit = {
    buf.verifyMagic("s2ac")
    val MaxVersion = 1
    val version = buf.getVersion(MaxVersion)
    configFormatHash = buf.getLong()
    configHash = buf.getLong()
    sourceHash = buf.getLong()
    sourceTimestamp = buf.getLong()
    buf.verifyMagic("E.ac")
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
