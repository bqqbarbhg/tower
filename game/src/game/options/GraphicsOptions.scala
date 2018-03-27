package game.options

import io.{SimpleSerializable, SimpleVisitor}
import render.opengl.MapMode

import GraphicsOptions._

object GraphicsOptions {

  class QualityOptions extends SimpleSerializable {

    var maxTextureSize: Int = 2048
    var shaderQuality: Int = 3
    var antialias: Int = 4
    var resolutionFactor: Double = 1.0
    var verticalSync: Boolean = true
    var halfFramerate: Boolean = false
    var highBitdepth: Boolean = true
    var preset: String = ""

    override def visit(v: SimpleVisitor): Unit = {
      maxTextureSize = v.field("maxTextureSize", maxTextureSize)
      shaderQuality = v.field("shaderQuality", shaderQuality)
      antialias = v.field("antialias", antialias)
      resolutionFactor = v.field("resolutionFactor", resolutionFactor)
      verticalSync = v.field("verticalSync", verticalSync)
      halfFramerate = v.field("halfFramerate", halfFramerate)
      preset = v.field("preset", preset)
    }

  }

  object QualityOptions {
    val Presets: Vector[(String, Option[() => QualityOptions])] = Vector(

      "Minimal" -> Some(() => new QualityOptions() {
        maxTextureSize = 256
        antialias = 1
        shaderQuality = 0
        halfFramerate = true
        highBitdepth = false
        preset = "Minimal"
      }),

      "Low" -> Some(() => new QualityOptions() {
        maxTextureSize = 512
        antialias = 2
        shaderQuality = 1
        preset = "Low"
      }),

      "Medium" -> Some(() => new QualityOptions() {
        maxTextureSize = 1024
        antialias = 4
        shaderQuality = 3
        preset = "Medium"
      }),

      "High" -> Some(() => new QualityOptions() {
        maxTextureSize = 2048
        antialias = 8
        shaderQuality = 3
        preset = "High"
      }),

      "Custom" -> None,

    )
  }

  class OpenGlOptions extends SimpleSerializable {

    var uniformMapMode: String = "Persistent"
    var vertexMapMode: String = "Persistent"
    var useUniformBuffers: Boolean = true
    var useVaoCache: Boolean = true
    var useRowMajorMatrices: Boolean = true
    var useImmutableTextureStorage: Boolean = true
    var preset: String = ""

    override def visit(v: SimpleVisitor): Unit = {
      uniformMapMode = v.field("uniformMapMode", uniformMapMode)
      vertexMapMode = v.field("vertexMapMode", vertexMapMode)
      useUniformBuffers = v.field("useUniformBuffers", useUniformBuffers)
      useVaoCache = v.field("useVaoCache", useVaoCache)
      useRowMajorMatrices = v.field("useRowMajorMatrices", useRowMajorMatrices)
      useImmutableTextureStorage = v.field("useImmutableTextureStorage", useImmutableTextureStorage)
      preset = v.field("preset", preset)
    }

  }

  object OpenGlOptions {
    val MapModes = Vector("SubData", "Map", "Persistent", "PersistentCopy", "PersistentCoherent", "PersistentCopyCoherent")

    val Presets: Vector[(String, Option[() => OpenGlOptions])] = Vector(

      "Ancient" -> Some(() => new OpenGlOptions() {
        uniformMapMode = "SubData"
        vertexMapMode = "SubData"
        useUniformBuffers = false
        useVaoCache = false
        useRowMajorMatrices = false
        useImmutableTextureStorage = false
        preset = "Ancient"
      }),

      "Compatible" -> Some(() => new OpenGlOptions() {
        uniformMapMode = "SubData"
        vertexMapMode = "Map"
        useUniformBuffers = true
        useVaoCache = true
        useRowMajorMatrices = false
        useImmutableTextureStorage = false
        preset = "Compatible"
      }),

      "Modern" -> Some(() => new OpenGlOptions() {
        uniformMapMode = "PersistentCopy"
        vertexMapMode = "Persistent"
        useUniformBuffers = true
        useVaoCache = true
        useRowMajorMatrices = true
        useImmutableTextureStorage = true
        preset = "Modern"
      }),

      "Custom" -> None,

    )

    def mapModeToEnum(name: String, fallback: MapMode): MapMode = {
      name match {
        case "SubData" => MapMode.SubData
        case "Map" => MapMode.Map
        case "Persistent" => MapMode.Persistent
        case "PersistentCopy" => MapMode.PersistentCopy
        case "PersistentCoherent" => MapMode.PersistentCoherent
        case "PersistentCopyCoherent" => MapMode.PersistentCopyCoherent
        case other =>
          println(s"Map mode '$name' does not exist!")
          fallback
      }
    }
  }

}

class GraphicsOptions extends SimpleSerializable {
  var quality = new QualityOptions()
  var openGl = new OpenGlOptions()

  override def visit(v: SimpleVisitor): Unit = {
    quality = v.field("quality", quality)
    openGl = v.field("openGl", openGl)
  }
}

