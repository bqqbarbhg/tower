package res.output

import java.io.File
import java.nio.ByteBuffer

import core._
import io.SimpleSerialization._
import org.lwjgl.system.MemoryUtil
import res.intermediate._
import util.BufferUtils._
import res.runner.OutputFileWriter

object EntityFile {

  def save(writer: OutputFileWriter, file: File, entity: EntitySpec): Unit = {
    // @Serialize(s2es)

    val buffer = MemoryUtil.memAlloc(64 * 1024 * 1024)

    def writeMap(map: SMap): Unit = {
      buffer.putInt(map.pairs.length)
      for ((k, v) <- map.pairs) {
        buffer.putString(k)
        v match {
          case SFloat(a) =>
            buffer.putInt(1)
            buffer.putDouble(a)
          case SInt(a) =>
            buffer.putInt(2)
            buffer.putInt(a.toInt)
          case SString(a) =>
            buffer.putInt(3)
            buffer.putString(a)
          case SBool(v) =>
            buffer.putInt(4)
            buffer.putInt(if (v) 1 else 0)
          case m: SMap =>
            buffer.putInt(5)
            writeMap(m)
          case other =>
            throw new RuntimeException(s"Unsupported data type: ${other.kind}")
        }
      }
    }

    val Version = 1
    buffer.putMagic("s2es")
    buffer.putVersion(Version)

    val static = entity.map("static") match {
      case SBool(v) => v
      case _ => false
    }

    var flags = 0x00
    if (static) flags |= 0x01

    buffer.putInt(flags)

    val multiMaps = entity.map.pairs.collect { case (s: String, m: SArray) => (s, m) }
    val multis = for {
      (k, v) <- multiMaps
      map <- v.data.collect { case m: SMap => m }
    } yield (k, map)

    val maps = entity.map.pairs.collect { case (s: String, m: SMap) => (s, m) }

    val all = maps ++ multis

    buffer.putInt(all.length)

    for ((k, v) <- all) {
      buffer.putString(k)
      writeMap(v)
    }

    buffer.putMagic("E.es")
    buffer.finish()

    writer.writeFile(file, buffer)

    MemoryUtil.memFree(buffer)
  }

}

