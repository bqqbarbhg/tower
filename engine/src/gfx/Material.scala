package gfx

import core._

object Material {
  lazy val missingAlbedo: Texture = withStack {
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

  lazy val missingNormal: Texture = withStack {
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
}

class Material {
  var albedoTexRes: Identifier = Identifier.Empty
  var normalTexRes: Identifier = Identifier.Empty
  var albedoTex: Texture = null
  var normalTex: Texture = null
}
