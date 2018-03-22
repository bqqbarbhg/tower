package res.process

import res.intermediate.Atlas.SpriteLocation
import res.intermediate._
import util.{Extents, Rectangle, RectanglePacker}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Packs multiple sprite-images into an atlas. One atlas consists of one or more
  * page-images which the sprites are pasted on and a mapping between the sprites
  * and their locations in the atlas.
  */
object GenerateAtlas {

  /** Generates increasing atlas page sizes */
  private def generateSizeAttemtps(maxSize: Int): Iterator[Extents] = {
    var size = 128
    var attempt = 0
    Iterator.continually({
      val result = attempt match {
        case 0 => Extents(size, size)
        case 1 => Extents(size * 2, size)
        case 2 => Extents(size, size * 2)
      }
      attempt += 1
      if (attempt == 3) {
        attempt = 0
        size *= 2
      }
      result
    }).takeWhile(p => p.w <= maxSize && p.h <= maxSize)
  }

  private case class PackResult(pageSize: Extents, packed: Map[Int, Rectangle])

  /** Try to pack sprites into a page */
  private def tryPack(packer: RectanglePacker, maxSize: Int, extents: Map[Int, Extents]): PackResult = {
    for (pageSize <- generateSizeAttemtps(maxSize)) {
      val packed = packer.pack(pageSize, extents)
      if (packed.size == extents.size) {
        return PackResult(pageSize, packed)
      }
    }

    val maxPage = new Extents(maxSize, maxSize)
    val packed = packer.pack(maxPage, extents)
    PackResult(maxPage, packed)
  }

  /** Returns required page sizes */
  private def packAtlas(atlas: Atlas, sprites: Seq[Sprite], cfg: Config.Res.Atlas): Seq[Extents] = {

    val packer = RectanglePacker.get(cfg.packingAlgorithm).getOrElse {
      println(s"ERROR: Packing algorithm not found: ${cfg.packingAlgorithm}, falling back to lightmap")
      RectanglePacker.get("lightmap").get
    }

    atlas.locations = new Array[Atlas.SpriteLocation](sprites.size)

    val toPack = mutable.HashMap[Int, Extents]()

    val extents = sprites.map(s => {
      var width = s.bounds.w + cfg.padding
      var height = s.bounds.h + cfg.padding
      if (s.wrapX) width += cfg.padding * 2
      if (s.wrapY) height += cfg.padding * 2

      Extents(width, height)
    })
    for ((extent, index) <- extents.zipWithIndex) {
      toPack(index) = extent
    }

    val pageSizes = ArrayBuffer[Extents]()

    var pageIndex = 0
    while (pageIndex < cfg.maxPages) {
      val PackResult(pageSize, packed) = tryPack(packer, cfg.maxPageSize, toPack.toMap)

      for ((index, rect) <- packed) {
        // Remove succesfully packed elements and store the location
        toPack -= index
        var rc = rect
        if (sprites(index).wrapX) rc = rc.copy(x = rc.x + cfg.padding)
        if (sprites(index).wrapY) rc = rc.copy(y = rc.y + cfg.padding)
        atlas.locations(index) = SpriteLocation(pageIndex, rc)
      }

      pageSizes += pageSize
      pageIndex += 1
      if (toPack.isEmpty) return pageSizes
    }

    Array[Extents]()
  }

  /** Try to generate an atlas, returns true on success */
  def generateAtlas(atlas: Atlas, sprites: Iterable[Sprite], config: Config.Res.Atlas): Boolean = {
    val spriteSeq = sprites.toSeq
    val pageSizes = packAtlas(atlas, spriteSeq, config)
    if (pageSizes.isEmpty) return false

    atlas.spriteImages = spriteSeq

    for (page <- pageSizes) {
      atlas.pages += Image.create32(page.w, page.h, config.srgb)
    }

    for ((loc, spriteIndex) <- atlas.locations.zipWithIndex) {
      val page = atlas.pages(loc.page)
      val sprite = spriteSeq(spriteIndex)

      val srcRect = sprite.bounds

      val wx = if (sprite.wrapX) config.padding else 0
      val wy = if (sprite.wrapY) config.padding else 0

      val bx = loc.rect.x
      val by = loc.rect.y
      for {
        yy <- (-wx) until (srcRect.h + wx)
        xx <- (-wy) until (srcRect.w + wy)
      } {
        val x = Math.floorMod(xx ,srcRect.w)
        val y = Math.floorMod(yy, srcRect.h)

        val pixel = sprite.image.getPixel(srcRect.x + x, srcRect.y + y)
        page.setPixel(bx + xx, by + yy, pixel)
      }
    }

    true
  }

}
