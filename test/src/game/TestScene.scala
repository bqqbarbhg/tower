package game

import java.io.File

import util.BufferUtils._
import java.nio.{ByteBuffer, ByteOrder}
import javax.management.openmbean.CompositeData
import javax.management.{Notification, NotificationEmitter, NotificationListener}

import audio._
import audio.effect.Limiter
import com.sun.management.GarbageCollectionNotificationInfo

import collection.JavaConverters._
import core._
import gfx.Shader.NoPermutations
import gfx._
import render._
import render.UniformBlock
import platform.{AppWindow, Intrinsic}
import ui.{Atlas, Font, Sprite, SpriteBatch}
import io.content._
import render.opengl._
import ui.Font.TextDraw

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

object TestScene extends App {

  println("Version 1.5")

  core.StackAllocator.createCurrentThread(16 * 1024 * 1024)

  val SampleRate = 44100
  val alOutput = new audio.output.OpenAlOutput(SampleRate, true)
  val fileOutput = new audio.output.FileAudioOutput("audiodump.bin")
  val audioOutput = new audio.output.MultiAudioOutput(
    Seq(
      alOutput,
      // fileOutput,
    )
  )

  val pack = new MultiPackage()

  JarPackage.create("data") match {
    case Some(jar) => pack.add(jar, 1)
    case None => // Nop
  }

  pack.add(new DirectoryPackage("data"), 0)

  Package.set(pack)

  object ModelTextures extends SamplerBlock {
    val Diffuse = sampler2D("Diffuse", Sampler.RepeatAnisotropic)
    val Normal = sampler2D("Normal", Sampler.RepeatAnisotropic)
  }

  object ModelUniform extends UniformBlock("ModelUniform") {
    val ViewProjection = mat4("ViewProjection")
    val Bones = mat4x3("Bones", 24)
  }

  val arg = util.ArgumentParser.parse(args)

  if (arg.flag("gl-no-ubo")) OptsGl.useUniformBlocks = false
  if (arg.flag("gl-no-vao")) OptsGl.useVaoCache = false

  if (arg.flag("gl-compat")) {
    OptsGl.uniformMap = MapMode.SubData
    OptsGl.vertexMap = MapMode.SubData
    OptsGl.useTexStorage = false
    OptsGl.useUniformBlocks = false
    OptsGl.useVaoCache = false
  }

  if (arg.flag("row-major")) {
    OptsGl.useRowMajorMatrix = true
  }

  if (arg.flag("debug")) {
    def printInfo(name: String, value: String): Unit = {
      println(f"$name%-20s = $value")
    }

    println("GL configuration:")
    printInfo("vertexMap", OptsGl.vertexMap.toString)
    printInfo("vertexMapFallback", OptsGl.vertexMapFallback.toString)
    printInfo("uniformMap", OptsGl.uniformMap.toString)
    printInfo("uniformMapFallback", OptsGl.uniformMapFallback.toString)
    printInfo("useTexStorage", OptsGl.useTexStorage.toString)
    printInfo("useUniformBlocks", OptsGl.useUniformBlocks.toString)
    printInfo("useVaoCache", OptsGl.useVaoCache.toString)
  }

  if (arg.flag("listen-gc")) {
    object GcListener extends NotificationListener {
      override def handleNotification(notification: Notification, handback: Any): Unit = {
        if (notification.getType == GarbageCollectionNotificationInfo.GARBAGE_COLLECTION_NOTIFICATION) {
          val info = GarbageCollectionNotificationInfo.from(notification.getUserData.asInstanceOf[CompositeData])
          println(s"GC [${info.getGcName}]: ${info.getGcAction}, cause: ${info.getGcCause}. Took ${info.getGcInfo.getDuration}ms")
        }
      }
    }

    val gcBeans = java.lang.management.ManagementFactory.getGarbageCollectorMXBeans().asScala
    for (gcBean <- gcBeans) {
      val emitter = gcBean.asInstanceOf[NotificationEmitter]
      emitter.addNotificationListener(GcListener, null, null)
    }
  }

  val music = Sound.load(Identifier("test/music.ogg.s2au")).get
  val musicInstance = new SoundInstance(music)
  val sound = Sound.load(Identifier("test/beep.wav.s2au")).get
  val soundInstance = new SoundInstance(sound)

  val mixer = new Mixer()

  mixer.add(musicInstance)
  // mixer.add(soundInstance)

  soundInstance.volume = 0.0
  soundInstance.copyParameters()
  soundInstance.setLoop()

  musicInstance.copyParameters()

  val limiter = new Limiter(mixer)

  val debug = arg.flag("debug")
  AppWindow.initialize(1280, 720, "Test window", debug)

  @volatile var closeAudio: Boolean = false

  def renderAudio(buffer: Array[Float], numFrames: Int): Unit = {
    TestScene.synchronized {
      limiter.advance(buffer, 0, numFrames, SampleRate)
    }
  }

  val audioThread = new Thread {
    override def run(): Unit = {
      // Disable denormals for audio performance
      Intrinsic.disableDenormals()

      core.StackAllocator.createCurrentThread(2 * 1024 * 1024)
      audioOutput.open()

      val MaxAudioLatency = 44100 / 10
      var framesWritten: Long = 0

      val BeginSilenceLength = 1500
      val EndSilenceLength = 1500
      val FadeInLength = 500
      val FadeOutLength = 500

      val ChunkSize = 1000
      val chunk = new Array[Float](ChunkSize * 2)

      {
        java.util.Arrays.fill(chunk, 0.0f)
        var silenceWritten = 0
        while (silenceWritten < BeginSilenceLength) {
          val toWrite = math.min(BeginSilenceLength - silenceWritten, ChunkSize)
          framesWritten += toWrite
          audioOutput.write(chunk, toWrite)
          silenceWritten += toWrite
        }
      }

      renderAudio(chunk, FadeInLength)
      framesWritten += FadeInLength
      for (i <- 0 until FadeInLength) {
        val s = i * 2
        val fade = i.toFloat / FadeInLength.toFloat
        chunk(s + 0) *= fade
        chunk(s + 1) *= fade
      }
      audioOutput.write(chunk, FadeInLength)

      audioOutput.start()

      while (!closeAudio) {
        val playPos = audioOutput.playbackFrame
        val lead = framesWritten - playPos
        val maxWrite = math.max(MaxAudioLatency - lead, 0)
        if (maxWrite > 0) {
          val toWrite = math.min(maxWrite, ChunkSize).toInt
          renderAudio(chunk, toWrite)
          framesWritten += toWrite
          audioOutput.write(chunk, toWrite)
        } else {
          Thread.sleep(5)
        }
      }

      renderAudio(chunk, FadeOutLength)
      framesWritten += FadeOutLength
      for (i <- 0 until FadeOutLength) {
        val s = i * 2
        val fade = 1.0f - i.toFloat / FadeOutLength.toFloat
        chunk(s + 0) *= fade
        chunk(s + 1) *= fade
      }
      audioOutput.write(chunk, FadeOutLength)

      {
        java.util.Arrays.fill(chunk, 0.0f)
        var silenceWritten = 0
        while (silenceWritten < EndSilenceLength) {
          val toWrite = math.min(EndSilenceLength - silenceWritten, ChunkSize)
          framesWritten += toWrite
          audioOutput.write(chunk, toWrite)
          silenceWritten += toWrite
        }
      }

      println("Closing audio")
      audioOutput.close()
    }
  }
  audioThread.start()


  val device = GraphicsDevice.get
  println(s"OS: ${java.lang.System.getProperty("os.name")}")
  println(s"Version: ${device.version}")
  println(s"Vendor: ${device.vendor}")
  println(s"Driver: ${device.driver}")

  if (device.doesNotSupportRowMajor) {
    OptsGl.useRowMajorMatrix = false
  }

  val renderer = Renderer.initialize()

  val shader = Shader.load("test/test_mesh", NoPermutations, ModelTextures, ModelUniform)

  val viewProjection = (
      Matrix4.perspective(1280.0 / 720.0, math.Pi / 3.0, 0.01, 1000.0)
      * Matrix43.look(Vector3(0.0, 0.0, -10.0), Vector3(0.0, 0.0, 1.0)))

  val buffer = ByteBuffer.allocateDirect(16 * 1024 * 1024)

  val model = gfx.Model.load(Identifier("test/sausageman/sausagemanWithTex.fbx.s2md")).get
  model.loadContent()

  val anim = model.anims.head

  val atlases = Package.get.list("atlas").filter(_.name.endsWith(".s2at"))
  for (atlas <- atlases) {
    println(s"Loading atlas: ${atlas.name}")
    Atlas.load(Identifier(atlas.name))
  }

  val sprite = Sprite.SpriteMap.get(Identifier("sprites/a.png"))
  Sprite.SpriteMap.atlases(sprite.atlas).loadTextures()

  val sb = new SpriteBatch()

  val font = Font.load("font/open-sans/OpenSans-Regular.ttf.s2ft").get

  var prevNs = java.lang.System.nanoTime()
  var sampledTimesMs = Array.fill(60/**100*/)(0.0)
  var ix = 0

  System.gc()

  val modelState = new ModelState(model)
  val animState = new AnimationState(model, anim)

  val startTime = AppWindow.currentTime
  var time = 0.0
  while (AppWindow.running) {
    val begin = java.lang.System.nanoTime()

    TestScene.synchronized {
      soundInstance.volume = 1.0 + math.max(math.sin(time * 1.0), 0.0) * 15.0
      // soundInstance.pan = math.sin(time * 5.0)
      soundInstance.copyParameters()
    }

    AppWindow.pollEvents()

    time = AppWindow.currentTime - startTime
    val world = Matrix43.translate(0.0, -4.0, 0.0) * Matrix43.rotateY(time * 0.5) * Matrix43.scale(0.01)

    renderer.advanceFrame()

    renderer.setDepthMode(true, true)
    renderer.clear(Some(Color.rgb(0x6495ED)), Some(1.0))

    shader.use()

    renderer.setDepthMode(true, true)
    renderer.setBlend(false)

    modelState.worldTransform = world
    animState.time = time % anim.duration
    animState.alpha = math.sin(time) * 0.5 + 0.5

    animState.apply(modelState)

    modelState.updateMatrices()

    for (mesh <- model.meshes) {
      renderer.setTexture(ModelTextures.Diffuse, mesh.material.albedoTex.texture)
      renderer.setTexture(ModelTextures.Normal, mesh.material.normalTex.texture)

      for (part <- mesh.parts) {

        renderer.pushUniform(ModelUniform, u => {
          import ModelUniform._
          ViewProjection.set(u, viewProjection)

          for ((name, index) <- part.boneName.zipWithIndex) {
            val node = model.findNodeByName(new Identifier(name))
            val transform = modelState.nodeWorldTransform(node) * part.boneMeshToBone(index)
            Bones.set(u, index, transform)
          }

        })

        part.draw()
      }
    }

    renderer.setDepthMode(false, false)
    renderer.setBlend(true)

    val fg = Color.rgb(0xffffff)
    val bg = Color.rgb(0x000000)

    val draws = ArrayBuffer[TextDraw]()
    draws += TextDraw("This is a long body text that", 0, "This is a long body text that".length, Vector2(100.0, 114.0), 22.0, fg, 0.0, 1)
    draws += TextDraw("Hello world!", 0, "Hello world!".length, Vector2(100.0, 90.0), 82.0, fg, 0.0, 1)
    draws += TextDraw("spans many lines but is currently", 0, "spans many lines but is currently".length, Vector2(100.0, 128.0), 22.0, fg, 0.0, 1)
    draws += TextDraw("Hello world!", 0, "Hello world!".length, Vector2(100.0, 90.0), 82.0, bg, 2.0, 0)
    draws += TextDraw("manually wrapped...", 0, "manually wrapped...".length, Vector2(100.0, 142.0), 22.0, fg, 0.0, 1)

    {
      val text = f"Volume: ${soundInstance.volume*100.0}%.0f%%"
      draws += TextDraw(text, 0, text.length, Vector2(100.0, 400.0), 30.0, Color.rgb(0xFFFFFF), 0.0, 0)
    }

    {
      val text = s"Row major: ${OptsGl.useRowMajorMatrix}"
      draws += TextDraw(text, 0, text.length, Vector2(100.0, 440.0), 30.0, Color.rgb(0xFFFFFF), 0.0, 0)
    }

    font.render(draws)

    val names = ('a' to 'z').map(_.toString) ++ Seq("face")
    for ((sprite, i) <- names.zipWithIndex) {
      val x = i % 6
      val y = i / 6
      val offset = Vector2(x, y) * 50.0
      val color = Color.rgb(0xffffff)
      val alpha = math.sin(time * 5.0 + i * 7.0) * 0.25 + 0.75
      val col = color.copy(a = alpha)
      sb.draw(Identifier(s"sprites/$sprite.png"), Vector2(800.0, 100.0) + offset, Vector2(50.0, 50.0), col)
    }

    sb.flush()

    AppWindow.swapBuffers()

    val end = java.lang.System.nanoTime()

    {
      var diff = end - begin
      val diffMs = diff.toDouble / 1000.0 / 1000.0
      sampledTimesMs(ix) = diffMs

      ix += 1
      if (ix == sampledTimesMs.length) {
        ix = 0
        val avg = sampledTimesMs.sum / sampledTimesMs.length
        val min = sampledTimesMs.reduce((a, b) => math.min(a, b))
        val max = sampledTimesMs.reduce((a, b) => math.max(a, b))
        println(f"Frame time: $min%.2fms - $max%.2fms, average $avg%.2fms")
      }
    }

  }
  AppWindow.unload()

  closeAudio = true
  audioThread.join()

}
