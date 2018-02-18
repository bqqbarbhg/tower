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

      /** Should the texture have mipmaps generated */
      var hasMipmaps: Boolean = true

      override def visit(v: SimpleVisitor): Unit = {
        semantic = v.field("semantic", semantic)
        compressed = v.field("compressed", compressed)
        hasMipmaps = v.field("hasMipmaps", hasMipmaps)
      }
    }

    class Animation extends SimpleSerializable {
      /** Maximum error value for the rotation keyframe compression */
      var rotationMaxError: Double = 0.01

      override def visit(v: SimpleVisitor): Unit = {
        rotationMaxError = v.field("rotationMaxError", rotationMaxError)
      }
    }

    class Sound extends SimpleSerializable {
      /** The sound is downsampled if it's sample rate is greater than this */
      var maxSampleRate: Int = 44100

      /** Forced resampling */
      var sampleRate: Int = 0

      /** Force to be mono */
      var mono: Boolean = false

      override def visit(v: SimpleVisitor): Unit = {
        maxSampleRate = v.field("maxSampleRate", maxSampleRate)
        sampleRate = v.field("sampleRate", sampleRate)
        mono = v.field("mono", mono)
      }
    }

    object Font {

      class Variant extends SimpleSerializable {

        /** Height of the variant (single) */
        var height: Int = 0

        /** Generate multiple sizes (minimum) */
        var heightMin: Int = 0
        /** Generate multiple sizes (maximum) */
        var heightMax: Int = 0
        /** Generate multiple sizes (step between sizes) */
        var heightInterval: Int = 1

        /** Amount to oversample in horizontal direction */
        var oversampleX: Int = 1

        /** Amount to oversample in vertical direction */
        var oversampleY: Int = 1

        /** Render the font into a signed distance field */
        var signedDistanceField: Boolean = false

        override def visit(v: SimpleVisitor): Unit = {
          height = v.field("height", height)
          heightMin = v.field("heightMin", heightMin)
          heightMax = v.field("heightMax", heightMax)
          heightInterval = v.field("heightInterval", heightInterval)
          oversampleX = v.field("oversampleX", oversampleX)
          oversampleY = v.field("oversampleY", oversampleY)
          signedDistanceField = v.field("signedDistanceField", signedDistanceField)
        }

      }

      class CharSet extends SimpleSerializable {

        /** Minimum unicode codepoint to process */
        var codepointMin: Int = 0

        /** Maximum unicode codepoint to process */
        var codepointMax: Int = 0

        override def visit(v: SimpleVisitor): Unit = {
          codepointMin = v.field("codepointMin", codepointMin)
          codepointMax = v.field("codepointMax", codepointMax)
        }
      }

    }

    class Font extends SimpleSerializable {

      /** Sizes and types to cook for the font */
      var variant = new ArrayBuffer[Font.Variant]()

      /** Ranges of characters to include */
      var charSet = new ArrayBuffer[Font.CharSet]()

      /** What algorithm to use to pack the characters */
      var packingAlgorithm: String = "lightmap"

      /** Maximum size of the texture */
      var maxSize: Int = 2048

      /** Texture options */
      var texture = new Texture()
      texture.hasMipmaps = false

      override def visit(v: SimpleVisitor): Unit = {
        variant = v.field("variant", variant, new Font.Variant)
        charSet = v.field("charSet", charSet, new Font.CharSet)
        packingAlgorithm = v.field("packingAlgorithm", packingAlgorithm)
        maxSize = v.field("maxSize", maxSize)
        texture = v.field("texture", texture)
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
    var sound = new Res.Sound()
    var font = new Res.Font()

    override def visit(v: SimpleVisitor): Unit = {
      texture = v.field("texture", texture)
      image = v.field("image", image)
      sprite = v.field("sprite", sprite)
      atlas = v.field("atlas", atlas)
      animation = v.field("animation", animation)
      sound = v.field("sound", sound)
      font = v.field("font", font)
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
