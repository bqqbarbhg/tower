package res

import io.{SimpleSerializable, SimpleVisitor}

import scala.collection.mutable.ArrayBuffer
import Config._

import scala.util.matching.Regex

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

  override def visit(v: SimpleVisitor): Unit = {
    filter = v.field("filter", filter, new Filter)
    importer = v.field("importer", importer)
    res = v.field("res", res)
    priority = v.field("priority", priority)
  }
}
