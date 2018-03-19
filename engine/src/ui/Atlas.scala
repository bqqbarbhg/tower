package ui

import java.nio.ByteBuffer

import core._
import gfx.Texture
import util.BufferUtils._
import Atlas._
import org.lwjgl.system.MemoryUtil
import ui.Sprite.AtlasIndexPair

object Atlas {

  object SpriteBounds extends Struct {
    val VertX0 = float
    val VertX1 = float
    val VertY0 = float
    val VertY1 = float

    val Aspect = float

    val UvBaseX = float
    val UvBaseY = float
    val UvScaleX = float
    val UvScaleY = float

    val Page = byte
  }

  def load(name: Identifier, atlasIndex: Int): Option[Atlas] = {
    io.content.Package.get.get(name).map(file => {
      val atlas = new Atlas(atlasIndex)

      val buffer = MemoryUtil.memAlloc(file.sizeInBytes.toInt)
      val stream = file.read()
      buffer.readFrom(stream)
      buffer.finish()
      atlas.load(buffer)
      stream.close()
      MemoryUtil.memFree(buffer)

      atlas
    })
  }

}

class Atlas(val atlasIndex: Int) {
  var data: ByteBuffer = null

  var textureArray: Option[Texture] = None
  var lastTexture: Option[Texture] = None

  var spriteNames = Array[Int]()
  var pageNames = Array[Int]()

  var spriteBase: Int = 0
  var numPages: Int = 0
  var numSprites: Int = 0

  def loadTextures(): Unit = {
    textureArray = Texture.createArray(pageNames.dropRight(1).map(new Identifier(_)))
    lastTexture = Texture.load(new Identifier(pageNames.last))
  }

  def load(buffer: ByteBuffer): Unit = {

    // @Deserialize(s2at)
    val MaxVersion = 1
    buffer.verifyMagic("s2at")
    val version = buffer.getVersion(MaxVersion)

    numSprites = buffer.getInt()
    numPages = buffer.getInt()

    val Seq(spriteBase, dataSize) = Struct.layout(SpriteBounds * numSprites)
    data = MemoryUtil.memAlloc(dataSize)
    this.spriteBase = spriteBase

    spriteNames = new Array[Int](numSprites)
    pageNames = new Array[Int](numPages)

    val D = data

    for (i <- 0 until numSprites) {
      val A = spriteBase + SpriteBounds.size * i

      val name = buffer.getIdentifier()
      val page = buffer.getInt()

      spriteNames(i) = name.index

      // Final page is not in the array, but in `finalTexture` so mark it
      // with special 0 index
      if (page == numPages - 1) {
        SpriteBounds.Page.set(D, A, 0)
      } else {
        SpriteBounds.Page.set(D, A, (page + 1).toByte)
      }

      val uvX0 = buffer.getShort()
      val uvY0 = buffer.getShort()
      val uvW = buffer.getShort()
      val uvH = buffer.getShort()
      val uvX1 = uvX0 + uvW
      val uvY1 = uvY0 + uvH

      val offsetX = buffer.getShort().toDouble
      val offsetY = buffer.getShort().toDouble
      val realW = buffer.getShort().toDouble
      val realH = buffer.getShort().toDouble

      val relX = offsetX / realW
      val relY = offsetY / realH
      val relW = uvW.toDouble / realW
      val relH = uvH.toDouble / realH

      val aspect = realW / realH
      SpriteBounds.Aspect.set(D, A, aspect.toFloat)

      val x0 = relX
      val x1 = relX + relW
      val y0 = relY
      val y1 = relY + relH
      SpriteBounds.VertX0.set(D, A, x0.toFloat)
      SpriteBounds.VertX1.set(D, A, x1.toFloat)
      SpriteBounds.VertY0.set(D, A, y0.toFloat)
      SpriteBounds.VertY1.set(D, A, y1.toFloat)

      val denX = x1 - x0
      val denY = y1 - y0
      SpriteBounds.UvScaleX.set(D, A, ((uvX1 - uvX0) / denX).toFloat)
      SpriteBounds.UvScaleY.set(D, A, ((uvY1 - uvY0) / denY).toFloat)
      SpriteBounds.UvBaseX.set(D, A, ((x1*uvX0 - x0*uvX1) / denX).toFloat)
      SpriteBounds.UvBaseY.set(D, A, ((y1*uvY0 - y0*uvY1) / denY).toFloat)

      val pair = AtlasIndexPair(atlasIndex, i)
      Sprite.SpriteMap.insert(name, pair)
    }

    for (i <- 0 until numPages) {
      val name = buffer.getIdentifier()
      pageNames(i) = name.index
    }

    buffer.verifyMagic("E.at")
  }

  def unload(): Unit = {
    lastTexture.foreach(_.unload())
    textureArray.foreach(_.unload())
    MemoryUtil.memFree(data)
  }

}

