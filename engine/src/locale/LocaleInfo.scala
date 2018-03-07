package locale

import java.nio.ByteBuffer
import core._
import util.BufferUtils._
import io.content.Package

object LocaleInfo {
  var locales: Seq[LocaleInfo] = Seq[LocaleInfo]()

  def load(): Unit = withStack {
    val pack = Package.get
    val localeFiles = pack.list("locale")
    val buffer = alloca(128)
    locales = for (localeFile <- localeFiles) yield {
      val stream = pack.get(localeFile.name).get.read()
      val info = new LocaleInfo(Identifier(localeFile.name))
      buffer.readFrom(stream)
      buffer.finish()
      info.load(buffer)
      buffer.finish()
      stream.close()
      info
    }
  }

}

class LocaleInfo(val file: Identifier) {
  var code: Identifier = Identifier.Empty
  var language: String = ""

  def load(buffer: ByteBuffer): Unit = {
    buffer.verifyMagic("s2lc")
    val MaxVersion = 1
    val version = buffer.getVersion(MaxVersion)

    buffer.verifyMagic("s2li")
    code = buffer.getIdentifier()
    language = buffer.getString()
    buffer.verifyMagic("E.li")
  }
}

