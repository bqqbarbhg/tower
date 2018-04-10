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
      /** How should this image be processed (texture, sprite, or colorgrade) */
      var ttype: String = ""

      /** Is the image sRGB data */
      var srgb: Boolean = true

      /** Image bit-depth */
      var colorDepth: Int = 8

      override def visit(v: SimpleVisitor): Unit = {
        ttype = v.field("type", ttype)
        srgb = v.field("srgb", srgb)
        colorDepth = v.field("colorDepth", colorDepth)
      }
    }

    class Sprite extends SimpleSerializable {
      /** Name of the atlas this sprite belongs to */
      var atlas: String = ""

      /** Allow cropping the transparent area of the sprite */
      var crop: Boolean = true

      /** Crop portions of the sprite only if all the channels are zero */
      var cropByAllChannels: Boolean = false

      /** Treat the sprite as a sprite-sheet animation */
      var animation: Boolean = false

      /** Aniamtion frames X */
      var framesX: Int = 0

      /** Aniamtion frames Y */
      var framesY: Int = 0

      /** Should the sprite be tiled horizontally */
      var wrapX: Boolean = false

      /** Should the sprite be tiled vertical */
      var wrapY: Boolean = false

      override def visit(v: SimpleVisitor): Unit = {
        atlas = v.field("atlas", atlas)
        crop = v.field("crop", crop)
        cropByAllChannels = v.field("cropByAllChannels", cropByAllChannels)
        animation = v.field("animation", animation)
        framesX = v.field("framesX", framesX)
        framesY = v.field("framesY", framesY)
        wrapX = v.field("wrapX", wrapX)
        wrapY = v.field("wrapY", wrapY)
      }
    }

    class Atlas extends SimpleSerializable {
      /** Which packing algorithm to use */
      var packingAlgorithm: String = "lightmap"

      /** Amount of padding per pixel */
      var padding: Int = 4

      /** Maximum number of pages in the atlas */
      var maxPages: Int = 32

      /** Maximum size of a page in the atlas */
      var maxPageSize: Int = 2048

      /** Is the image generated for the atlas in sRGB */
      var srgb: Boolean = true

      /** How to process the page textures */
      var texture = new Texture()
      texture.premultiplyAlpha = true
      texture.readAsLinear = true

      override def visit(v: SimpleVisitor): Unit = {
        packingAlgorithm = v.field("packingAlgorithm", packingAlgorithm)
        padding = v.field("padding", padding)
        maxPages = v.field("maxPages", maxPages)
        maxPageSize = v.field("maxPageSize", maxPageSize)
        srgb = v.field("srgb", srgb)
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

      /** Should the texture data be converted from sRGB to linar in shaders */
      var readAsLinear: Boolean = false

      /** Should the colors be premultiplied with alpha */
      var premultiplyAlpha: Boolean = false

      /** Never downscale this image */
      var noDownscale: Boolean = false

      /** Bits per pixel of the texture */
      var colorDepth: Int = 8

      /** Number of channels in the resulting image in RGBA order */
      var channels: Int = 4

      override def visit(v: SimpleVisitor): Unit = {
        semantic = v.field("semantic", semantic)
        compressed = v.field("compressed", compressed)
        hasMipmaps = v.field("hasMipmaps", hasMipmaps)
        readAsLinear = v.field("readAsLinear", readAsLinear)
        premultiplyAlpha = v.field("premultiplyAlpha", premultiplyAlpha)
        noDownscale = v.field("noDownscale", noDownscale)
        colorDepth = v.field("colorDepth", colorDepth)
        channels = v.field("channels", channels)
      }
    }

    class Animation extends SimpleSerializable {

      /** Maximum error value for the rotation keyframe compression */
      var rotationMaxError: Double = 0.01

      /** Maximum error value for the position keyframe compression */
      var positionMaxError: Double = 0.001

      /** Maximum error value for the scale keyframe compression */
      var scaleMaxError: Double = 0.001

      override def visit(v: SimpleVisitor): Unit = {
        rotationMaxError = v.field("rotationMaxError", rotationMaxError)
        positionMaxError = v.field("positionMaxError", positionMaxError)
        scaleMaxError = v.field("scaleMaxError", scaleMaxError)
      }
    }

    class Mesh extends SimpleSerializable {

      /** Maximum number of bone weights per vertex */
      var maxBonesPerVertex: Int = 4

      /** Maximum number of bones in one mesh (draw call) */
      var maxBonesPerMesh: Int = 24

      /** Minimum amount of bone influence allowed before it's culled */
      var boneCullWeight: Double = 0.01

      override def visit(v: SimpleVisitor): Unit = {
        maxBonesPerVertex = v.field("maxBonesPerVertex", maxBonesPerVertex)
        maxBonesPerMesh = v.field("maxBonesPerMesh", maxBonesPerMesh)
        boneCullWeight = v.field("boneCullWeight", boneCullWeight)
      }
    }

    object Model {

      class Node extends SimpleSerializable {

        /** Filter for node name */
        var name: String = ""

        /** Disables automatic world transform calculation */
        var auxilary: Boolean = false

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
          name = v.field("name", name)
          auxilary = v.field("auxilary", auxilary)
        }
      }

      class Mesh extends SimpleSerializable {

        /** Filter for node name */
        var name: String = ""

        /** Texture name to use for the mesh (overrides material) */
        var texture: String = ""

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
          name = v.field("name", name)
          texture = v.field("texture", texture)
        }

      }

    }


    class Model extends SimpleSerializable {

      /** Global scaling to apply to the model's root transform */
      var scale: Double = 1.0

      /** Node-specific configuration */
      var nodes: ArrayBuffer[Model.Node] = new ArrayBuffer[Model.Node]()

      /** Mesh-specific configuration */
      var meshes: ArrayBuffer[Model.Mesh] = new ArrayBuffer[Model.Mesh]()

      override def visit(v: SimpleVisitor): Unit = {
        scale = v.field("scale", scale)
        nodes = v.field("nodes", nodes, new Model.Node())
        meshes = v.field("meshes", meshes, new Model.Mesh())
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

        /** SDF only: Maximum width of the outline relative to the character size */
        var maxOutlineRelative: Double = 0.0

        override def visit(v: SimpleVisitor): Unit = {
          height = v.field("height", height)
          heightMin = v.field("heightMin", heightMin)
          heightMax = v.field("heightMax", heightMax)
          heightInterval = v.field("heightInterval", heightInterval)
          oversampleX = v.field("oversampleX", oversampleX)
          oversampleY = v.field("oversampleY", oversampleY)
          signedDistanceField = v.field("signedDistanceField", signedDistanceField)
          maxOutlineRelative = v.field("maxOutlineRelative", maxOutlineRelative)
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
      texture.noDownscale = true

      override def visit(v: SimpleVisitor): Unit = {
        variant = v.field("variant", variant, new Font.Variant)
        charSet = v.field("charSet", charSet, new Font.CharSet)
        packingAlgorithm = v.field("packingAlgorithm", packingAlgorithm)
        maxSize = v.field("maxSize", maxSize)
        texture = v.field("texture", texture)
      }
    }

    class Shader extends SimpleSerializable {

      /** Root directory in asset path where to search for shader imports */
      var importPath: String = ""

      override def visit(v: SimpleVisitor): Unit = {
        importPath = v.field("importPath", importPath)
      }
    }

    class Colorgrade extends SimpleSerializable {

      /** Resolution of the color lookup table */
      var resolution: Int = 0

      /** Texture options */
      var texture = new Texture()
      texture.compressed = false
      texture.hasMipmaps = false
      texture.colorDepth = 16

      override def visit(v: SimpleVisitor): Unit = {
        resolution = v.field("resolution", resolution)
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
    var mesh = new Res.Mesh()
    var model = new Res.Model()
    var sound = new Res.Sound()
    var font = new Res.Font()
    var shader = new Res.Shader()
    var colorgrade = new Res.Colorgrade()

    override def visit(v: SimpleVisitor): Unit = {
      texture = v.field("texture", texture)
      image = v.field("image", image)
      sprite = v.field("sprite", sprite)
      atlas = v.field("atlas", atlas)
      animation = v.field("animation", animation)
      mesh = v.field("mesh", mesh)
      model = v.field("model", model)
      sound = v.field("sound", sound)
      font = v.field("font", font)
      shader = v.field("shader", shader)
      colorgrade = v.field("colorgrade", colorgrade)
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
