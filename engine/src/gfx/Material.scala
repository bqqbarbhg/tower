package gfx

import core._

object Material {
  lazy val missingAlbedo: Texture = withStack {
    val data = alloca(4)
    data.put(0xFF.toByte)
    data.put(0xFF.toByte)
    data.put(0xFF.toByte)
    data.put(0xFF.toByte)
    data.finish()
    Texture.createRgba(1, 1, data)
  }

  lazy val missingNormal: Texture = withStack {
    val data = alloca(4)
    data.put(0x80.toByte)
    data.put(0x80.toByte)
    data.put(0x00.toByte)
    data.put(0x00.toByte)
    data.finish()
    Texture.createRgba(1, 1, data)
  }
}

class Material {
  var albedoTexRes: Identifier = Identifier.Empty
  var normalTexRes: Identifier = Identifier.Empty
  var albedoTex: Texture = null
  var normalTex: Texture = null
}
