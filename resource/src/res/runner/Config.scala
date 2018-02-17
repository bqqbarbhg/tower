package res.runner

import io.{SimpleSerializable, SimpleVisitor}
import res.runner.Config._
import core._

import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex
import io.SimpleSerialization.SMap
import org.lwjgl.system.MemoryStack

object Config {

  object Filter {

    /** Convert a file glob like 'folder/file_*.txt' to a regex like 'folder/file_[^/]*\.txt'. */
    def globToRegex(glob: String): Regex = {
      val regexStr = glob.replace("**", "&").flatMap(c => {
        c match {
          case '&' => ".*"
          case '*' => "[^/]*"
          case '.' => "\\."
          case '(' => "\\("
          case ')' => "\\)"
          case '[' => "\\["
          case ']' => "\\]"
          case '+' => "\\+"
          case '|' => "\\|"
          case '^' => "\\$"
          case '$' => "\\$"
          case '@' => "\\@"
          case '%' => "\\%"
          case c => c.toString
        }
      })
      ("^" + regexStr + "$").r
    }
  }

  /** Asset configs can be conditional to only some assets */
  class Filter extends SimpleSerializable {
    var filename: String = ""
    var name: String = ""

    private var cachedFilenameRegexSrc = ""
    private var cachedFilenameRegex: Option[Regex] = None
    def filenameRegex: Option[Regex] = {
      if (cachedFilenameRegexSrc != filename) {
        cachedFilenameRegexSrc = filename
        cachedFilenameRegex = Some(Filter.globToRegex(filename))
      }
      cachedFilenameRegex
    }

    private var cachedNameRegexSrc = ""
    private var cachedNameRegex: Option[Regex] = None
    def nameRegex: Option[Regex] = {
      if (cachedNameRegexSrc != name) {
        cachedNameRegexSrc = name
        cachedNameRegex = Some(Filter.globToRegex(name))
      }
      cachedNameRegex
    }

    override def visit(v: SimpleVisitor): Unit = {
      filename = v.field("filename", filename)
      name = v.field("name", name)
    }
  }

  /** Importer specific settings */
  class Importer extends SimpleSerializable {
    var name: String = ""

    override def visit(v: SimpleVisitor): Unit = {
      name = v.field("name", name)
    }
  }

  object Res {

    class Image extends SimpleSerializable {
      /** How should this image be processed (texture or sprite) */
      var ttype: String = ""

      override def visit(v: SimpleVisitor): Unit = {
        ttype = v.field("type", ttype)
      }
    }

    class Texture extends SimpleSerializable {
      /** What is this texture used for */
      var semantic: String = "color"

      /** Should the texture be compressed */
      var compressed: Boolean = false

      override def visit(v: SimpleVisitor): Unit = {
        semantic = v.field("semantic", semantic)
        compressed = v.field("compressed", compressed)
      }
    }

  }

  /** Resource type specific settings */
  class Res extends SimpleSerializable {
    var texture = new Res.Texture()
    var image = new Res.Image()

    override def visit(v: SimpleVisitor): Unit = {
      texture = v.field("texture", texture)
    }
  }

}

/** Root of the asset configuration file */
class Config extends SimpleSerializable {
  var filter = new ArrayBuffer[Filter]()
  var importer = new Importer()
  var res = new Res()
  var priority = 0.0

  /** Hash value of this configuration */
  def calculateHash: Long = withStack {
    val buf = alloca(16 * 1024)
    io.UncheckedUtil.writeToBytes(buf, this)
    buf.finish()
    util.BufferHash.hashBuffer(buf)
  }

  override def visit(v: SimpleVisitor): Unit = {
    filter = v.field("filter", filter, new Filter)
    importer = v.field("importer", importer)
    res = v.field("res", res)
    priority = v.field("priority", priority)
  }
}

/**
  * A .toml config file.
  *
  * @param file Source .toml file
  * @param config Deserialized configuration for _this_ config
  * @param root Serialized version of the config, required for merging multiple configs
  */
case class ConfigFile(file: File, config: Config, root: SMap)
