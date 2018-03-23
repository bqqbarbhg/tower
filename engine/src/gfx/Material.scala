package gfx

import asset.{DynamicAsset, Unloadable}
import core._

object Material {

  class Shared extends Unloadable {
    val missingAlbedo: Texture = withStack {
      val data = alloca(4)
      data.put(0x80.toByte)
      data.put(0x80.toByte)
      data.put(0x80.toByte)
      data.put(0x80.toByte)
      data.finish()
      val tex = Texture.createRgba(1, 1, data, true)
      tex.texture.setLabel("MissingAlbedo")
      tex
    }

    val missingNormal: Texture = withStack {
      val data = alloca(4)
      data.put(0x80.toByte)
      data.put(0x80.toByte)
      data.put(0x00.toByte)
      data.put(0x00.toByte)
      data.finish()
      val tex = Texture.createRgba(1, 1, data, false)
      tex.texture.setLabel("MissingNormal")
      tex
    }

    def unload(): Unit = {
      missingAlbedo.unload()
      missingNormal.unload()
    }
  }

  val shared = DynamicAsset("Material.Shared", new Shared)
}

class Material {
  var albedoTexRes: Identifier = Identifier.Empty
  var normalTexRes: Identifier = Identifier.Empty
  var albedoTex: Texture = null
  var normalTex: Texture = null
}
