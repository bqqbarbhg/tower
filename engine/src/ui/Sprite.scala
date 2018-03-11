package ui

import core.Identifier

import scala.collection.mutable.ArrayBuffer
import Sprite._
import asset.AtlasAsset

object Sprite {

  object AtlasIndexPair {
    val Invalid = AtlasIndexPair(-1, -1)
    def apply(atlas: Int, index: Int) = new AtlasIndexPair(atlas.toLong << 32 | index.toLong)
  }

  class AtlasIndexPair(val value: Long) extends AnyVal {
    def valid: Boolean = atlas >= 0
    def atlas: Int = (value >>> 32).toInt
    def index: Int = (value & (1L << 32) - 1).toInt
  }

  /**
    * A linear-probe insert-only hash-map where the hash function is identity
    * of the identifier index.
    */
  object SpriteMap {

    private var identifiers = Array[Int]()
    private var atlasIndexPairs = Array[Long /* = AtlasIndexPair */]()
    private var numSprites = 0

    var atlasAssets = new ArrayBuffer[AtlasAsset]()

    /** Clear the map of all sprites */
    def clear(): Unit = {
      identifiers = Array[Int]()
      atlasIndexPairs = Array[Long]()
      atlasAssets.clear()
      numSprites = 0
    }

    /** Return the initial position where to start scanning */
    def hash(identifier: Identifier): Int = {
      // Leave some gaps to make the scan distances more even?
      identifier.index * 2
    }

    /**
      * Insert a sprite into the mapping.
      * It is forbidden to add two sprites with the same name or duplcate sprites.
      */
    def insert(name: Identifier, pair: AtlasIndexPair): Unit = {
      // If load capacity is over 50% re-hash
      if (numSprites * 2 >= identifiers.length) {
        val oldIdentifiers = identifiers
        val oldPairs = atlasIndexPairs
        val newSize = math.max(numSprites * 2, 32)
        identifiers = new Array[Int](newSize)
        atlasIndexPairs = new Array[Long](newSize)
        numSprites = 0

        for ((rawIdentifier, rawPair) <- (oldIdentifiers zip oldPairs)) {
          val identifier = new Identifier(rawIdentifier)
          val pair = new AtlasIndexPair(rawPair)
          insert(identifier, pair)
        }
      }

      // Scan for empty slot
      var pos = hash(name) % identifiers.length
      while (identifiers(pos) != 0) {
        pos = (pos + 1) % identifiers.length
      }

      identifiers(pos) = name.index
      atlasIndexPairs(pos) = pair.value
    }

    /**
      * Find a sprite from the mapping.
      * Returns `AtlasIndexPair.Invalid` (`pair.valid == false`) on failure.
      */
    def get(name: Identifier): AtlasIndexPair = {

      // Note: In the insertion the scan speed didn't really matter, but this
      // is called during gameplay, so the modulus is replaced with a binary and:
      // `index % length` is the same as `index & (length - 1)` when `length`
      // is a power of two. Also keep the identifiers in a local variable to
      // potentially optimize the bytecode.
      val ix = name.index
      val ids = identifiers
      val mask = identifiers.length - 1
      var pos = hash(name) & mask
      while (ids(pos) != ix) {
        // Null-entry -> key is not found
        if (ids(pos) == 0) return AtlasIndexPair.Invalid
        pos = (pos + 1) & mask
      }

      val rawPair = atlasIndexPairs(pos)
      new AtlasIndexPair(rawPair)
    }
  }

}

