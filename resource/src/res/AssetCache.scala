package res

import java.nio.ByteBuffer

import util.BufferIntegrityException
import util.BufferUtils._

class AssetCache {
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
}
