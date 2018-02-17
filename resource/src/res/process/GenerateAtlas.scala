package res.process

import res.intermediate.Atlas.SpriteLocation
import res.intermediate._
import util.{Extents, Rectangle, RectanglePacker}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

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
  private def packAtlas(atlas: Atlas, images: Seq[Image], config: Config): Seq[Extents] = {
    val cfg = config.res.atlas

    val packer = RectanglePacker.get(cfg.packingAlgorithm).getOrElse {
      println(s"ERROR: Packing algorithm not found: ${cfg.packingAlgorithm}, falling back to lightmap")
      RectanglePacker.get("lightmap").get
    }

    atlas.locations = new Array[Atlas.SpriteLocation](images.size)

    val toPack = mutable.HashMap[Int, Extents]()

    val extents = images.map(i => new Extents(i.width + cfg.padding, i.height + cfg.padding))
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
        atlas.locations(index) = SpriteLocation(pageIndex, rect)
      }

      pageSizes += pageSize
      pageIndex += 1
      if (toPack.isEmpty) return pageSizes
    }

    Array[Extents]()
  }

  /** Try to generate an atlas, returns true on success */
  def generateAtlas(atlas: Atlas, images: Iterable[Image], config: Config): Boolean = {
    val imageSeq = images.toSeq
    val pageSizes = packAtlas(atlas, imageSeq, config)
    if (pageSizes.isEmpty) return false

    for (page <- pageSizes) {
      atlas.pages += Image.create32(page.w, page.h, true)
    }

    for ((loc, imageIndex) <- atlas.locations.zipWithIndex) {
      val page = atlas.pages(loc.page)
      val image = imageSeq(imageIndex)

      val bx = loc.rect.x
      val by = loc.rect.y
      for {
        y <- 0 until image.height
        x <- 0 until image.width
      } {
        val pixel = image.getPixel(x, y)
        page.setPixel(bx + x, by + y, pixel)
      }
    }

    true
  }

}
