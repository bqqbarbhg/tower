package res.runner

import java.io.File
import java.nio.file.Paths

import io.UncheckedUtil
import org.lwjgl.BufferUtils
import res.runner.Runner._
import util.BufferUtils._
import util.{BufferHash, BufferIntegrityException}
import core._

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
  var assetsToProcess = Vector[AssetFile]()

  val byteBuffer = BufferUtils.createByteBuffer(1024*1024*128)

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
      val buf = byteBuffer.duplicateEx
      buf.readFromFile(temp)
      buf.finish()
      cache.read(buf)
    } catch {
      case e: BufferIntegrityException =>
        if (opts.verbose) println(s"> $relPath: Cache integrity fail: ${e.getMessage}")
        return true
    }

    if (cache.configFormatHash != configFormatHash) {
      if (opts.verbose) println(s"> $relPath: Config _format_ hash changed")
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

        if (config.importer.name.nonEmpty) {
          allAssets += new AssetFile(assetFile, config, configs)
        }
      }
    }

    // Find the assets that have changed
    {
      println(s"Determining assets to process... ${allAssets.length} found")
      val dirtyAssets = allAssets.filter(isAssetDirty).toVector
    }

    // Process the assets!
    {
      println(s"Processing assets... ${assetsToProcess.length} found")
      for (asset <- assetsToProcess) {
        val cache = new AssetCacheFile()
        cache.configFormatHash = configFormatHash

        cache.configHash = asset.config.calculateHash
        cache.sourceHash = BufferHash.hashFile(asset.file)
        cache.sourceTimestamp = asset.file.lastModified()

        {
          val relPath = assetRelative(asset.file)
          val cacheFile = Paths.get(opts.tempRoot, relPath + ".s2ac").toFile
          cacheFile.getParentFile.mkdirs()
          val buf = byteBuffer.duplicateEx
          cache.write(buf)
          buf.finish()
          buf.writeToFile(cacheFile)
        }
      }
    }
  }

}
