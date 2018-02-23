package res.process

import core._
import res.intermediate._
import res.intermediate.Font._
import res.intermediate.BakedFont._
import util.{BinarySearch, Extents, RectanglePacker}

import scala.collection.mutable.ArrayBuffer
import scala.reflect.macros.whitebox

/**
  * Bakes a vector font into a bitmap version. The configuration defines
  * different sized variants for the font that are processed.
  */
object BakeFont {

  private class VariantToBuild(val variant: Variant, val bitmapsIndices: Map[Char, Int])

  def bakeVariantBitmaps(font: Font, height: Int, variant: Variant, charset: Set[Char]): Map[Char, Bitmap] = {
    val config = variant.config
    val pairs = for (char <- charset) yield {
      val bitmap = if (config.signedDistanceField) {
        val opts = variant.sdfOptions.get
        font.renderGlyphSdf(char, height, opts.step, opts.edgeValue, opts.padding)
      } else {
        font.renderGlyphAa(char, height, config.oversampleX, config.oversampleY)
      }
      (char, bitmap)
    }

    pairs.toMap
  }

  def bakeFont(font: Font, config: Config.Res.Font): BakedFont = {

    val packer = RectanglePacker.get(config.packingAlgorithm).getOrElse {
      println(s"ERROR: Packing algorithm not found: ${config.packingAlgorithm}, falling back to lightmap")
      RectanglePacker.get("lightmap").get
    }

    // Resolve the supported charset
    val fullCharset = (config.charSet
      .map(set => (set.codepointMin to set.codepointMax).toSet)
      .fold(Set[Int]())(_ ++ _)
      .map(_.toChar))

    val glyphs = fullCharset.toIterable.flatMap(c => font.getGlyph(c).map(g => (c, g))).toMap
    val charset = glyphs.keySet

    val variantsToBuild = new ArrayBuffer[VariantToBuild]()

    // Collect all the bitmaps for the various variants
    val bitmaps = new ArrayBuffer[Bitmap]()
    for (variant <- config.variant) {
      val heights = if (variant.height > 0) {
        Some(variant.height).toSeq
      } else if (variant.heightMin > 0 && variant.heightMax >= variant.heightMin && variant.heightInterval > 0) {
        (variant.heightMin to variant.heightMax by variant.heightInterval).toSeq
      } else {
        throw new RuntimeException("Misconfigured font variant: no height determinable!")
      }

      for (height <- heights) {
        val sizedVariant = new Variant(height, variant)
        val charToBitmap = bakeVariantBitmaps(font, height, sizedVariant, charset)
        val indices = charToBitmap.map({ case (char, bitmap) =>
          val index = bitmaps.length
          bitmaps += bitmap
          (char, index)
        })

        sizedVariant.scale = font.getScaleForHeight(height)
        variantsToBuild += new VariantToBuild(sizedVariant, indices)
      }
    }

    val sizes = bitmaps.zipWithIndex.map(p => (p._2, new Extents(p._1.width + 1, p._1.height + 1))).toMap

    /** Try to pack the font into an atlas sized `width` x `height` */
    def tryPack(width: Int, height: Int): Boolean = {
      val extents = new Extents(width, height)
      var sizesLeft = sizes
      for (channel <- 0 until 4) {
        val result = packer.pack(extents, sizesLeft)
        sizesLeft --= result.keys
      }
      sizesLeft.isEmpty
    }

    /** Iteratively double the width until it fits */
    def findSmallestPossibleWidth(): Option[Int] = {
      var width = 32
      while (width < config.maxSize) {
        if (tryPack(width, width)) return Some(width)
        width *= 2
      }
      None
    }

    // Pack the bitmaps into an atlas
    val width = findSmallestPossibleWidth() match {
      case Some(width) => width
      case None => throw new RuntimeException(s"Failed to fit the font into ${config.maxSize}x${config.maxSize}}")
    }

    // Binary search for the minimum height that fits
    val height = BinarySearch.upperBound(32, width, height => tryPack(width, height))

    val finalExtents = new Extents(width, height)
    val rects = new Array[GlyphRect](bitmaps.length)

    // Do the final packing
    {
      var sizesLeft = sizes
      for (channel <- 0 until 4) {
        val result = packer.pack(finalExtents, sizesLeft)
        sizesLeft --= result.keys

        for ((index, rect) <- result) {
          val bitmap = bitmaps(index)
          val offset = Vector2(bitmap.x, bitmap.y)
          val cropped = rect.copy(w = rect.w - 1, h = rect.h - 1)
          rects(index) = GlyphRect(offset, cropped, channel)
        }
      }
      assert(sizesLeft.isEmpty)
    }

    // Blit the image
    val image = Image.create32(width, height, srgb = false)
    for ((bitmap, rect) <- (bitmaps zip rects)) {
      for {
        y <- 0 until bitmap.height
        x <- 0 until bitmap.width
      } {
        val ix = rect.rect.x + x
        val iy = rect.rect.y + y
        val pixel = image.getPixel(ix, iy)
        val value = bitmap.getPixel(x, y)
        val newPixel = rect.channel match {
          case 0 => pixel.copy(r = value)
          case 1 => pixel.copy(g = value)
          case 2 => pixel.copy(b = value)
          case 3 => pixel.copy(a = value)
        }
        image.setPixel(ix, iy, newPixel)
      }
    }

    val kerningPairs = (for {
      prev <- charset
      next <- charset
    } yield {
      val pair = (prev, next)
      (pair, font.getKerning(prev, next))
    }).filter(pair => math.abs(pair._2) > 0.0001).toMap

    val bakedFont = new BakedFont()
    bakedFont.image = image
    bakedFont.variants = variantsToBuild.map(variant => {
      variant.variant.glyphs = variant.bitmapsIndices.mapValues(rects)
      variant.variant
    }).toVector
    bakedFont.kerningPairs = kerningPairs
    bakedFont.glyphs = glyphs
    bakedFont.scalePerPixel = font.getScaleForHeight(1.0)

    bakedFont
  }
}
