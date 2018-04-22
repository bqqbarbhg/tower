package locale

import java.nio.ByteBuffer
import core._
import util.BufferUtils._
import io.content.Package

object LocaleInfo {
  var locales: Seq[LocaleInfo] = Seq[LocaleInfo]()
  var defaultLocale: LocaleInfo = null

  def load(): Unit = withStack {
    val pack = Package.get
    val localeFiles = pack.list("locale").filter(_.name.endsWith(".s2lc"))
    val buffer = alloca(128)
    
    locales = for (localeFile <- localeFiles) yield {
      val stream = pack.get(localeFile.name).get.read()
      val info = new LocaleInfo(Identifier(localeFile.name))
      val buf = buffer.sliceEx
      buf.readFrom(stream)
      buf.finish()
      info.load(buffer)
      stream.close()
      info
    }

    val idEn = Identifier("en")
    defaultLocale = locales.find(_.code == idEn).getOrElse(locales.head)
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

