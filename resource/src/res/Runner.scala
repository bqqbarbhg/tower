package res

import java.io.File

import collection.mutable.ArrayBuffer
import Runner._
import io.SimpleSerialization.SMap

object Runner {
  case class ConfigFile(file: File, config: Config, root: SMap)
  case class AssetFile(file: File, config: Config)

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
  val assets = new ArrayBuffer[AssetFile]()

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
        if (opts.verbose) println(s"> ${assetRelative(assetFile)} [${config.importer.name}]")

        if (config.importer.name.nonEmpty) {
          assets += AssetFile(assetFile, config)
        }
      }
    }

    // Process assets
    {
      println(s"Processing assets... ${assets.length} found")
    }
  }

}
