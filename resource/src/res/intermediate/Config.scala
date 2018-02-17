package res.intermediate

import java.io.File

import core._
import io.SimpleSerialization.SMap
import io.{SimpleSerializable, SimpleVisitor}
import res.intermediate.Config._

import scala.collection.mutable.ArrayBuffer
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

      /** Is the image sRGB data */
      var srgb: Boolean = true

      override def visit(v: SimpleVisitor): Unit = {
        ttype = v.field("type", ttype)
        srgb = v.field("srgb", srgb)
      }
    }

    class Sprite extends SimpleSerializable {
      /** Name of the atlas this sprite belongs to */
      var atlas: String = ""

      override def visit(v: SimpleVisitor): Unit = {
        atlas = v.field("atlas", atlas)
      }
    }

    class Atlas extends SimpleSerializable {
      /** Which packing algorithm to use */
      var packingAlgorithm: String = "lightmap"

      /** Amount of padding per pixel */
      var padding: Int = 4

      /** Maximum number of pages in the atlas */
      var maxPages: Int = 10

      /** Maximum size of a page in the atlas */
      var maxPageSize: Int = 2048

      /** How to process the page textures */
      var texture = new Texture()

      override def visit(v: SimpleVisitor): Unit = {
        packingAlgorithm = v.field("packingAlgorithm", packingAlgorithm)
        padding = v.field("padding", padding)
        maxPages = v.field("maxPages", maxPages)
        maxPageSize = v.field("maxPageSize", maxPageSize)
        texture = v.field("texture", texture)
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

    class Animation extends SimpleSerializable {
      /** Maximum error value for the rotation keyframe compression */
      var rotationMaxError: Double = 0.01

      override def visit(v: SimpleVisitor): Unit = {
        rotationMaxError = v.field("rotationMaxError", rotationMaxError)
      }
    }

  }

  /** Resource type specific settings */
  class Res extends SimpleSerializable {
    var texture = new Res.Texture()
    var image = new Res.Image()
    var sprite = new Res.Sprite()
    var atlas = new Res.Atlas()
    var animation = new Res.Animation()

    override def visit(v: SimpleVisitor): Unit = {
      texture = v.field("texture", texture)
      image = v.field("image", image)
      sprite = v.field("sprite", sprite)
      animation = v.field("animation", animation)
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
