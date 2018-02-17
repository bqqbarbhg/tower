package res.runner

import java.io.File
import java.nio.file.Paths

import io.UncheckedUtil
import org.lwjgl.BufferUtils
import res.runner.Runner._
import util.BufferUtils._
import util.{BufferHash, BufferIntegrityException}
import res.importer.Importer
import res.intermediate.{AssetFile, Config, ConfigFile}

import core._
import res.intermediate._
import res.process._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


object Runner {

  /** Apply configs successively in priority order */
  def mergeConfigs(configs: Vector[ConfigFile]): Config = {
    val sorted = configs.sortBy(-_.config.priority)
    val merged = new Config()
    for (conf <- sorted) {
      conf.root.write(merged)
    }
    merged
  }
}

/**
  * The main resource processing runner.
  */
class Runner(val opts: RunOptions) {
  print(s"Listing all files in '${opts.assetRoot}'...")
  Console.flush()
  val assetRoot = new File(opts.assetRoot).getCanonicalFile
  val sourceFiles = io.PathUtil.listFilesRecursive(assetRoot).sortBy(_.getAbsolutePath).toVector
  println(s" ${sourceFiles.length} found")

  val absoluteAssetPath = assetRoot.getAbsolutePath
  def assetRelative(file: File): String = {
    val absolute = file.getAbsolutePath
    absolute.drop(absoluteAssetPath.length + 1).replace('\\', '/')
  }

  val configs = new ArrayBuffer[ConfigFile]()
  val allAssets = new ArrayBuffer[AssetFile]()

  val atlases = new mutable.HashMap[String, Atlas]()

  val configFormatHash = UncheckedUtil.fieldHash(new Config)

  /**
    * Apply all the configs that are relevant to `file`
    */
  def getConfigs(file: File): Vector[ConfigFile] = {
    val fileRel = assetRelative(file)
    configs.filter(config => {
      val configRel = assetRelative(config.file)
      // /config/ folder applies to everyone
      configRel.startsWith("config/") || {
        // Otherwise only ones which are lower in the hierarchy
        val lastSep = configRel.lastIndexOf('/')
        val configDir = if (lastSep >= 0) configRel.take(lastSep + 1) else ""
        fileRel.startsWith(configDir)
      }
    }).filter(config => {
      // Check for filename filters
      val configRel = assetRelative(config.file)
      val filenameFilters = (for {
        filt <- config.config.filter
        regex <- filt.filenameRegex
      } yield regex)
      filenameFilters.isEmpty || filenameFilters.exists(_.findFirstIn(fileRel).isDefined)
    }).filter(config => {
      // Check for name filters
      val name = file.getName
      val lastDot = name.lastIndexOf('.')
      val nameNoExt = if (lastDot >= 0) name.take(lastDot) else name
      val nameFilters = (for {
        filt <- config.config.filter
        regex <- filt.nameRegex
      } yield regex)
      nameFilters.isEmpty || nameFilters.exists(_.findFirstIn(nameNoExt).isDefined)
    }).toVector
  }

  /**
    * Checks whether an `AssetFile` needs to be processed depending on the current settings.
    * Note: Even if the asset is not dirty it may need to be reprocessed if some asset depends
    * on it is dirty.
    */
  def isAssetDirty(asset: AssetFile): Boolean = {
    val relPath = assetRelative(asset.file)
    if (!opts.skipOnTimestamp && !opts.skipOnHash) {
      if (opts.verbose) println(s"> $relPath: Skipping disabled")
      return true
    }

    // Locate the cache file
    val temp = Paths.get(opts.tempRoot, relPath + ".s2ac").toFile

    if (!temp.exists) {
      if (opts.verbose) println(s"> $relPath: Cache file doesn't exist")
      return true
    }

    if (!temp.isFile || !temp.canRead) {
      if (opts.verbose) println(s"> $relPath: Not a readable file")
      return true
    }

    val cache = new AssetCacheFile()
    try {
      cache.load(temp)
    } catch {
      case e: BufferIntegrityException =>
        if (opts.verbose) println(s"> $relPath: Cache integrity fail: ${e.getMessage}")
        return true
    }

    if (cache.configFormatHash != configFormatHash) {
      if (opts.verbose) println(s"> $relPath: Config format hash changed")
      return true
    }

    val configHash = asset.config.calculateHash
    if (cache.configHash != configHash) {
      if (opts.verbose) println(s"> $relPath: Config hash changed")
      return true
    }

    if (opts.skipOnTimestamp && cache.sourceTimestamp != asset.file.lastModified) {
      if (opts.verbose) println(s"> $relPath: File timestamp changed")
      return true
    }

    // Timestamp is fine and that's all we care about
    if (opts.skipOnTimestamp) return false

    val sourceHash = BufferHash.hashFile(asset.file)
    if (cache.sourceHash != sourceHash) {
      if (opts.verbose) println(s"> $relPath: Source file hash changed")
      return true
    }

    // `opts.skipOnHash` must be set since it's checked in the beginning
    // so since the hash matches this file can be skipped
    false
  }

  def run(): Unit = {

    // Load configurations
    {
      val tomlFiles = sourceFiles.filter(_.getName().endsWith(".toml"))
      println(s"Parsing config .toml files... ${tomlFiles.length} found")
      for (tomlFile <- tomlFiles) {
        if (opts.verbose) println(s"> ${assetRelative(tomlFile)}")
        try {
          val root = io.Toml.parseFile(tomlFile.getAbsolutePath)
          val config = new Config()
          root.write(config)
          configs += ConfigFile(tomlFile, config, root)
        } catch {
          case e: io.TomlParseException => println(e.getMessage)
          case e: io.SimpleSerializeException => println(e.getMessage)
        }
      }
    }

    // Resolve assets
    {
      val assetFiles = sourceFiles.filterNot(_.getName().endsWith(".toml"))
      println(s"Resolving assets... ${assetFiles.length} found")
      for (assetFile <- assetFiles) {
        val configs = getConfigs(assetFile)
        for (config <- configs) {
          if (opts.debug) println(s"  - ${assetRelative(config.file)}")
        }

        val config = mergeConfigs(configs)
        if (opts.verbose && config.importer.name.nonEmpty)
          println(s"> ${assetRelative(assetFile)} [${config.importer.name}]")

        val importerName = config.importer.name
        if (importerName.nonEmpty) {
          Importer.get(importerName) match {
            case Some(importer) =>
              importer.importType.maskConfig(config)
              allAssets += new AssetFile(assetFile, config, configs)
            case None =>
              println(s"${assetRelative(assetFile)} ERROR: Importer not found: '$importerName'")
          }

        }
      }
    }

    // Find the assets that have changed
    {
      println(s"Determining assets to process... ${allAssets.length} found")
      for (dirtyAsset <- allAssets.filter(isAssetDirty)) {
        dirtyAsset.hasChanged = true
      }
    }

    // Collect atlases
    {
      for (asset <- allAssets.filter(_.config.res.image.ttype == "sprite")) {
        val atlasName = asset.config.res.sprite.atlas
        if (atlasName.isEmpty) {
          println(s"${assetRelative(asset.file)} ERROR: Sprite has no atlas set")
        } else {
          val atlas = atlases.getOrElseUpdate(atlasName, new Atlas(atlasName))
          atlas.sprites += asset
          if (asset.hasChanged) atlas.hasChanged = true
          if (opts.debug) println(s"Sprite ${assetRelative(asset.file)} -> $atlasName")
        }
      }
    }

    // Process atlases
    {
      val dirtyAtlases = atlases.values.filter(_.hasChanged)
      println(s"Processing atlases... ${dirtyAtlases.size} found")
      for (atlas <- dirtyAtlases) {
        if (opts.verbose) println(s"> ${atlas.name} (${atlas.sprites.length} sprites)")
        val sprites = atlas.sprites.flatMap(_.importAsset())
        val images = sprites.map(_.asInstanceOf[Image])
        assert(images.nonEmpty && images.size == atlas.sprites.size)

        val config = atlas.sprites.head.config
        if (GenerateAtlas.generateAtlas(atlas, images, config.res.atlas)) {
          for ((page, index) <- atlas.pages.zipWithIndex) {
            val name = s"${atlas.name}_$index.png"
            val file = Paths.get(opts.tempRoot, "atlas", name).toFile
            file.getAbsoluteFile.getParentFile.getCanonicalFile.mkdirs()
            SaveDebugImage.saveImage(file, page)
          }

        } else {
          println(s"Atlas ${atlas.name} ERROR: Failed to pack")
        }

        for (page <- atlas.pages) {
        }

        sprites.foreach(_.unload())
        atlas.unload()
      }
    }

    // Process the assets!
    {
      val updated = allAssets.filter(_.hasChanged)
      println(s"Updating asset cache... ${updated.length} found")
      for (asset <- updated) {
        val cache = new AssetCacheFile()
        cache.configFormatHash = configFormatHash

        cache.configHash = asset.config.calculateHash
        cache.sourceHash = BufferHash.hashFile(asset.file)
        cache.sourceTimestamp = asset.file.lastModified()

        {
          val relPath = assetRelative(asset.file)
          val cacheFile = Paths.get(opts.tempRoot, relPath + ".s2ac").toFile
          cacheFile.getParentFile.mkdirs()
          cache.save(cacheFile)
        }
      }
    }
  }

}
