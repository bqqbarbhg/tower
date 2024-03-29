package res.runner

import java.io.File
import java.nio.file.Paths
import java.util.concurrent.{Executor, ExecutorService, Executors, LinkedBlockingQueue}

import io.UncheckedUtil
import org.lwjgl.BufferUtils
import res.runner.Runner._
import util.BufferUtils._
import util.{BufferHash, BufferIntegrityException}
import res.intermediate.{AssetFile, Config, ConfigFile}
import core._
import process.HyphenateLocale
import res.importer._
import res.intermediate._
import res.output._
import res.process._
import res.runner.Runner._

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

  protected case class TaskResult(name: String, duration: Double)
}

/**
  * The main resource processing runner.
  */
class Runner(val opts: RunOptions) {
  if (opts.verbose)
    print(s"Listing all files in '${opts.assetRoot}'...")
  Console.flush()
  val assetRoot = new File(opts.assetRoot).getCanonicalFile
  val sourceFiles = io.PathUtil.listFilesRecursive(assetRoot).sortBy(_.getAbsolutePath).toVector
  if (opts.verbose)
    println(s" ${sourceFiles.length} found")

  val absoluteAssetPath = assetRoot.getAbsolutePath
  def assetRelative(file: File): String = {
    val absolute = file.getAbsolutePath
    absolute.drop(absoluteAssetPath.length + 1).replace('\\', '/')
  }

  val configs = new ArrayBuffer[ConfigFile]()
  val allAssets = new ArrayBuffer[AssetFile]()
  val dirtyAssets = new ArrayBuffer[AssetFile]()

  val atlases = new mutable.HashMap[String, Atlas]()

  val configFormatHash = UncheckedUtil.fieldHash(new Config)

  val writer = new OutputFileWriter(opts)

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

    val fileSize = asset.file.length()
    if (cache.sourceSize != fileSize) {
      if (opts.verbose) println(s"> $relPath: File size changed")
      return true
    }

    val importerVersion = asset.fileType.version
    if (cache.importerVersion != importerVersion) {
      if (opts.verbose) println(s"> $relPath: Importer version changed ${cache.importerVersion} -> $importerVersion")
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

  def numThreads: Int = if (opts.numThreads > 0)
    opts.numThreads
  else
    Runtime.getRuntime.availableProcessors

  protected var taskExecutor: ExecutorService = null
  protected var numTasks: Int = 0
  protected val resultQueue = new LinkedBlockingQueue[TaskResult]()

  class TaskToRun(name: String, task: () => Unit) extends Runnable {
    override def run(): Unit = {
      StackAllocator.createCurrentThreadIfNecessary(16*1024*1024)
      val startTime = System.nanoTime()

      try {
        task()
      } catch {
        case ex: Exception =>
          println(s"Failed to process task $name: ${ex.getMessage}")
          ex.printStackTrace()
      }

      val endTime = System.nanoTime()
      val duration = (endTime - startTime).toDouble * 1e-9
      resultQueue.offer(TaskResult(name, duration))
    }
  }

  protected def addTask(name: String)(task: => Unit): Unit = {
    numTasks += 1
    taskExecutor.submit(new TaskToRun(name, () => task))
  }

  protected def addAssetTask(asset: AssetFile)(task: => Unit): Unit = {
    val name = assetRelative(asset.file)
    addTask(name)(task)
  }

  protected def finishTasks(): Unit = {
    val numStart = numTasks

    println(s"Processing tasks: $numStart ($numThreads threads)")

    while (numTasks > 0) {
      val result = resultQueue.take()
      numTasks -= 1

      val numDone = numStart - numTasks
      val prefix = s"($numDone/$numStart)"
      println(f"$prefix%s ${result.name}%s: ${result.duration}%.2fs")
    }
  }

  def run(): Unit = {
    taskExecutor = Executors.newFixedThreadPool(numThreads)

    // Load configurations
    {
      val tomlFiles = sourceFiles.filter(_.getName().endsWith(".ac.toml"))
      if (opts.verbose)
        println(s"Parsing config .ac.toml files... ${tomlFiles.length} found")
      for (tomlFile <- tomlFiles) {
        if (opts.debug) println(s"> ${assetRelative(tomlFile)}")
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
      val assetFiles = sourceFiles.filterNot(_.getName().endsWith(".ac.toml"))
      if (opts.verbose)
        println(s"Resolving assets... ${assetFiles.length} found")
      for (assetFile <- assetFiles) {
        val configs = getConfigs(assetFile)
        for (config <- configs) {
          if (opts.debug) println(s"  - ${assetRelative(config.file)}")
        }

        val config = mergeConfigs(configs)
        if (opts.debug && config.importer.name.nonEmpty)
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
      if (opts.verbose)
        println(s"Determining assets to process... ${allAssets.length} found")
      for (dirtyAsset <- allAssets.filter(isAssetDirty)) {
        dirtyAsset.hasChanged = true
        dirtyAssets += dirtyAsset
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
          atlas.spriteAssets += asset
          if (asset.hasChanged) atlas.hasChanged = true
          if (opts.debug) println(s"Sprite ${assetRelative(asset.file)} -> $atlasName")
        }
      }
    }

    // Process atlases
    {
      val dirtyAtlases = atlases.values.filter(_.hasChanged)
      if (opts.verbose)
        println(s"Processing atlases... ${dirtyAtlases.size} found")
      for (atlas <- dirtyAtlases) addTask(s"Atlas: ${atlas.name}") {
        if (opts.verbose) println(s"> ${atlas.name} (${atlas.spriteAssets.length} sprites)")
        val spriteAssets = atlas.spriteAssets.flatMap(_.importAsset())
        val spriteImages = spriteAssets.map(_.asInstanceOf[Image])
        assert(spriteImages.nonEmpty && spriteImages.size == atlas.spriteAssets.size)

        val sprites = (for ((file, image) <- (atlas.spriteAssets zip spriteImages)) yield {
          ProcessSprite.processSprite(image, assetRelative(file.file), file.config.res.sprite)
        }).flatten

        val config = atlas.spriteAssets.head.config
        if (GenerateAtlas.generateAtlas(atlas, sprites, config.res.atlas)) {

          val pageFileBase = Paths.get(opts.dataRoot, "atlas", s"${atlas.name}").toFile
          val pageNameBase = writer.dataRelative(pageFileBase)

          for ((page, index) <- atlas.pages.zipWithIndex) {
            val filename = Paths.get(opts.tempRoot, "atlas", s"${atlas.name}_$index.png").toFile
            filename.getParentFile.mkdirs()
            SaveDebugImage.saveImage(filename, page)
          }

          for ((page, index) <- atlas.pages.zipWithIndex) {
            val texture = CreateTexture.createTexture(page, config.res.atlas.texture)
            val filename = Paths.get(opts.dataRoot, "atlas", s"${atlas.name}_$index.s2tx").toFile
            TextureFile.save(writer, filename, texture)
            texture.unload()
          }

          val filename = Paths.get(opts.dataRoot, "atlas", s"${atlas.name}.s2at").toFile
          AtlasFile.save(writer, filename, atlas, pageNameBase)
        } else {
          println(s"Atlas ${atlas.name} ERROR: Failed to pack")
        }

        sprites.foreach(_.unload())
        spriteImages.foreach(_.unload())
        atlas.unload()
      }
    }

    // Process textures
    {
      val dirtyTextures = dirtyAssets.filter(_.config.res.image.ttype == "texture")
      if (opts.verbose)
        println(s"Processing textures... ${dirtyTextures.size} found")
      for (asset <- dirtyTextures) addAssetTask(asset) {
        val resources = asset.importAsset()
        assert(resources.size == 1)
        val image = resources.head.asInstanceOf[Image]

        val texture = CreateTexture.createTexture(image, asset.config.res.texture)
        val relPath = assetRelative(asset.file)
        val filename = Paths.get(opts.dataRoot, relPath + ".s2tx").toFile
        TextureFile.save(writer, filename, texture)
        texture.unload()
        image.unload()
      }
    }

    // Process colorgrades
    {
      val dirtyTextures = dirtyAssets.filter(_.config.res.image.ttype == "colorgrade")
      if (opts.verbose)
        println(s"Processing textures... ${dirtyTextures.size} found")
      for (asset <- dirtyTextures) addAssetTask(asset) {
        val resources = asset.importAsset()
        assert(resources.size == 1)
        val image = resources.head.asInstanceOf[Image]

        val colorTex = ProcessColorgrade.processColorgrade(image, asset.config.res.colorgrade)
        val relPath = assetRelative(asset.file)
        val filename = Paths.get(opts.dataRoot, relPath + ".s2tx").toFile
        TextureFile.save(writer, filename, colorTex)

        colorTex.unload()
        image.unload()
      }
    }

    // Process PCM sounds
    {
      val dirtyPcms = dirtyAssets.filter(_.fileType == ImportFilePcm)
      if (opts.verbose)
        println(s"Processing PCM sounds... ${dirtyPcms.size} found")
      for (asset <- dirtyPcms) addAssetTask(asset) {
        val resources = asset.importAsset()
        assert(resources.size == 1)
        val pcm = resources.head.asInstanceOf[PcmSound]
        val sound = PcmProcess.processPcm(pcm, asset.config.res.sound)
        pcm.unload()

        val relPath = assetRelative(asset.file)
        val filename = Paths.get(opts.dataRoot, relPath + ".s2au").toFile
        AudioFile.save(writer, filename, sound)
        sound.unload()
      }
    }

    // Process audio sounds
    {
      val dirtySounds = dirtyAssets.filter(_.fileType == ImportFileAudio)
      if (opts.verbose)
        println(s"Processing compressed sounds... ${dirtySounds.size} found")
      for (asset <- dirtySounds) addAssetTask(asset) {
        val resources = asset.importAsset()
        assert(resources.size == 1)
        val sound = resources.head.asInstanceOf[Sound]

        val relPath = assetRelative(asset.file)
        val filename = Paths.get(opts.dataRoot, relPath + ".s2au").toFile
        AudioFile.save(writer, filename, sound)
        sound.unload()
      }
    }

    // Process fonts
    {
      def validFont(asset: AssetFile): Boolean = {
        val cfg = asset.config.res.font
        cfg.charSet.nonEmpty && cfg.variant.nonEmpty
      }
      val dirtyFonts = dirtyAssets.filter(validFont)
      if (opts.verbose)
        println(s"Processing fonts... ${dirtyFonts.size} found")
      for (asset <- dirtyFonts) addAssetTask(asset) {
        val relPath = assetRelative(asset.file)
        val resources = asset.importAsset()
        assert(resources.size == 1)
        val font = resources.head.asInstanceOf[Font]

        val bakedFont = BakeFont.bakeFont(font, asset.config.res.font)

        for ((chan, index) <- "rgba".zipWithIndex) {
          val filename = Paths.get(opts.tempRoot, relPath + s"_$chan.png").toFile
          val file = filename.getCanonicalFile.getAbsoluteFile
          file.getParentFile.mkdirs()
          SaveDebugImage.saveImageChannel(file, bakedFont.image, index)
        }

        val texRes = {
          val texture = CreateTexture.createTexture(bakedFont.image, asset.config.res.font.texture)
          val filename = Paths.get(opts.dataRoot, relPath + ".s2tx").toFile
          TextureFile.save(writer, filename, texture)
          texture.unload()
          writer.dataRelative(filename)
        }

        {
          val filename = Paths.get(opts.dataRoot, relPath + ".s2ft").toFile
          FontFile.save(writer, filename, bakedFont, texRes)
        }

        bakedFont.unload()
        font.unload()
      }
    }

    // Process models
    {
      val textureAssets = allAssets.filter(_.config.res.image.ttype == "texture").map(_.file.getCanonicalFile.getAbsolutePath).toSet
      val dirtyModels = dirtyAssets.filter(_.fileType == ImportFileModel)
      if (opts.verbose)
        println(s"Processing models... ${dirtyModels.size} found")
      for (asset <- dirtyModels) addAssetTask(asset) {
        val relPath = assetRelative(asset.file)
        val resources = asset.importAsset()
        var meshes = resources.collect({ case a: Mesh => a }).toSeq
        val animations = resources.collect({ case a: Animation => a }).toSeq
        val models = resources.collect({ case a: Model => a }).toSeq

        val parent = asset.file.getParentFile
        val siblingFiles = parent.listFiles.filterNot(_.isDirectory).toSeq

        val siblingTextures = siblingFiles.filter(file => {
          val absolute = file.getCanonicalFile.getAbsolutePath
          textureAssets.contains(absolute)
        }).map(_.getName())

        def resolveTextureFile(name: String): String = {
          val file = Paths.get(parent.getCanonicalFile.getAbsolutePath, name).toFile
          val assetRel = assetRelative(file)
          val dataRel = Paths.get(opts.dataRoot, assetRel).toFile
          writer.dataRelative(dataRel)
        }

        PatchModelProperties.patchModelProperties(meshes, asset.config.res.model)
        PatchAnimationProperties.patchAnimationProperties(animations, asset.config.res.model)

        for (mesh <- meshes) {
          mesh.material = ResolveMaterial.resolveMaterialFromTexture(mesh.textureName, siblingTextures, resolveTextureFile)
        }

        meshes = JoinMesh.joinMeshes(models.head, meshes, asset.config.res.model)

        val meshMapping = (for (mesh <- meshes) yield {
          val parts = ProcessMesh.processMesh(mesh, asset.config.res.mesh)

          val filename = Paths.get(opts.dataRoot, s"$relPath.${mesh.name}.s2ms").toFile
          MeshFile.save(writer, filename, parts)
          parts.foreach(_.unload())

          (mesh.name, (writer.dataRelative(filename), mesh.material))
        }).toMap

        val animMapping = (for (anim <- animations) yield {
          ProcessAnimation.processAnimation(anim, asset.config.res.animation)

          val filename = Paths.get(opts.dataRoot, s"$relPath.${anim.name}.s2an").toFile
          AnimationFile.save(writer, filename, anim)

          (anim.name, writer.dataRelative(filename))
        }).toMap

        for (model <- models) {
          ScaleModel.scaleModel(model, asset.config.res.model)

          val flatModel = FlattenModel.flattenModel(model, meshMap = meshMapping, animMap = animMapping, asset.config.res.model)

          val filename = Paths.get(opts.dataRoot, s"$relPath.s2md").toFile
          ModelFile.save(writer, filename, flatModel)
          flatModel.unload()
        }

        resources.foreach(_.unload())
      }
    }

    // Process shaders
    {
      val dirtyShaders = dirtyAssets.filter(_.fileType == ImportFileShader)
      if (opts.verbose)
        println(s"Processing shaders... ${dirtyShaders.size} found")

      for (asset <- dirtyShaders) addAssetTask(asset) {
        val relPath = assetRelative(asset.file)
        val resources = asset.importAsset()
        assert(resources.size == 1)
        val shader = resources.head.asInstanceOf[Shader]
        val processed = PreprocessShader.preprocessShader(shader, asset.config.res.shader)

        val filename = Paths.get(opts.dataRoot, s"$relPath.s2sh").toFile
        ShaderFile.save(writer, filename, processed)

        shader.unload()
      }
    }

    // Process locales
    {
      val dirtyLocales = dirtyAssets.filter(_.fileType == ImportFileLocale)
      if (opts.verbose)
        println(s"Processing locales... ${dirtyLocales.size} found")

      for (asset <- dirtyLocales) addAssetTask(asset) {
        val relPath = assetRelative(asset.file)
        val resources = asset.importAsset()
        assert(resources.size == 1)
        val locale = resources.head.asInstanceOf[Locale]
        HyphenateLocale.hyphenateLocale(locale, assetRoot.getAbsolutePath)

        val flatLocale = FlattenLocale.flattenLocale(locale)

        val filename = Paths.get(opts.dataRoot, s"$relPath.s2lc").toFile
        LocaleFile.save(writer, filename, flatLocale)

        flatLocale.unload()
        locale.unload()
      }
    }

    // Process entities
    {
      val dirtyEntities = dirtyAssets.filter(_.fileType == ImportFileEntity)
      if (opts.verbose)
        println(s"Processing entities... ${dirtyEntities.size} found")

      for (asset <- dirtyEntities) addAssetTask(asset) {
        val relPath = assetRelative(asset.file)
        val resources = asset.importAsset()
        assert(resources.size == 1)
        val entity = resources.head.asInstanceOf[EntitySpec]

        val filename = Paths.get(opts.dataRoot, s"$relPath.s2es").toFile
        EntityFile.save(writer, filename, entity)

        entity.unload()
      }
    }

    // Game data root
    {
      val filename = Paths.get(opts.dataRoot, s"data_root.s2dr").toFile
      DataRootFile.save(writer, filename)
    }

    // Wait for the processing to end
    finishTasks()

    // Update the asset cache
    {
      val updated = allAssets.filter(_.hasChanged)
      if (opts.verbose)
        println(s"Updating asset cache... ${updated.length} found")
      for (asset <- updated) {
        val cache = new AssetCacheFile()
        cache.configFormatHash = configFormatHash

        cache.configHash = asset.config.calculateHash
        cache.sourceHash = BufferHash.hashFile(asset.file)
        cache.sourceSize = asset.file.length
        cache.sourceTimestamp = asset.file.lastModified()
        cache.importerVersion = asset.fileType.version

        {
          val relPath = assetRelative(asset.file)
          val cacheFile = Paths.get(opts.tempRoot, relPath + ".s2ac").toFile
          cacheFile.getParentFile.mkdirs()
          cache.save(cacheFile)
        }
      }
    }

    taskExecutor.shutdown()
  }

}
