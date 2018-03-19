package game

import asset._
import audio.effect.Limiter
import audio.{Mixer, SoundInstance}
import core._
import game.test.{Sausageman, TestAudioEngine}
import gfx.{ModelState, Shader}
import input._
import render._
import io.content._
import io.Toml
import locale._
import main.EngineStartup
import platform.AppWindow
import res.runner.{RunOptions, Runner}
import ui.Font.TextDraw
import ui.SpriteBatch

import collection.mutable.ArrayBuffer

object TestEngine extends App {

  StackAllocator.createCurrentThread(16 * 1024 * 1024)

  val arg = util.ArgumentParser.parse(args,
    implicitArgumentFlags = Vector("process"),
    multiArgumentFlags = Vector("process"),
    aliases = Map("P" -> "process"))

  val process = arg.multiKeywords("process")
  val processOpts = if (process.nonEmpty) {
    Some(RunOptions.createFromFiles(process))
  } else None

  def processResources(): Unit = {
    for (opts <- processOpts) {
      val runner = new Runner(opts)
      runner.run()
    }
  }

  processResources()

  object SimpleShader extends ShaderAsset("test/test_simple") {

    override object Permutations extends Shader.Permutations {
      val UseBones = vert("UseBones", 0 to 1)
    }

    override object Textures extends SamplerBlock {
      import Sampler._

      val AlbedoTex = sampler2D("AlbedoTex", RepeatTrilinear)
      val NormalTex = sampler2D("NormalTex", RepeatTrilinear)
    }

    uniform(VertexGlobal)
    object VertexGlobal extends UniformBlock("GlobalUniform") {
      val ViewProjection = mat4("ViewProjection")
    }

    uniform(VertexInstance)
    object VertexInstance extends UniformBlock("ModelUniform") {
      val World = mat4x3("World")
    }

    uniform(VertexInstanceBones)
    object VertexInstanceBones extends UniformBlock("ModelUniformBones") {
      val Bones = mat4x3("Bones", 24)
    }
  }

  val pack = new MultiPackage()

  JarPackage.create("data") match {
    case Some(jar) => pack.add(jar, 1)
    case None => // Nop
  }

  pack.add(new DirectoryPackage("data"), 0)

  Package.set(pack)

  AssetLoader.preloadAtlases()

  val sausagemanAsset = ModelAsset("test/sausageman/sausagemanWithTex.fbx.s2md")
  val fontAsset = FontAsset("font/open-sans/OpenSans-Regular.ttf.s2ft")
  val uiAtlas = AtlasAsset("atlas/foo.s2at")
  val music = SoundAsset("test/music_test.ogg.s2au")

  val opts = new EngineStartup.Options()
  opts.debug = true
  opts.windowName = "Engine test"
  EngineStartup.start(opts)

  val bundle = new AssetBundle(SimpleShader, sausagemanAsset, uiAtlas)
  bundle.acquire()

  val SampleRate = 44100
  val alOutput = new audio.output.OpenAlOutput(SampleRate, true)
  val fileOutput = new audio.output.FileAudioOutput("audiodump.bin")
  val audioOutput = new audio.output.MultiAudioOutput(
    Seq(
      alOutput,
      // fileOutput,
    )
  )

  val mixer = new Mixer()
  val limiter = new Limiter(mixer)

  def renderAudio(buffer: Array[Float], numFrames: Int): Unit = {
    TestEngine.synchronized {
      // limiter.advance(buffer, 0, numFrames, SampleRate)
    }
  }

  val audioEngine = new TestAudioEngine(audioOutput, renderAudio)

  val musicInstance = new SoundInstance(music.get)
  musicInstance.copyParameters()
  mixer.add(musicInstance)

  val LC = TestLocale
  LocaleInfo.load()

  {
    val code = Identifier("fi")
    Locale.load(LocaleInfo.locales.find(_.code == code).getOrElse(LocaleInfo.locales.head))
  }

  val sausageman = new Sausageman(sausagemanAsset)
  val sb = new SpriteBatch()

  var frameCount = 0

  val renderer = Renderer.get
  var prevWidth = -1
  var prevHeight = -1

  object DebugInput extends InputSet("Debug") {
    val Reload = button("Reload")
  }

  val keyboard = AppWindow.keyboard
  val debugMapping = Toml.parse(
    """
      |[Keyboard.Debug]
      |Reload = "R"
    """.stripMargin)

  val mapping = new InputMapping(Array(keyboard))
  mapping.init(debugMapping)

  val startTime = AppWindow.currentTime
  while (AppWindow.running) {
    frameCount += 1
    AppWindow.pollEvents()
    val time = AppWindow.currentTime - startTime

    val viewWidth = AppWindow.width
    val viewHeight = AppWindow.height

    val viewProjection = (
      Matrix4.perspective(viewWidth.toDouble / viewHeight.toDouble, math.Pi / 3.0, 0.01, 1000.0)
        * Matrix43.look(Vector3(0.0, 8.0, -14.0), Vector3(0.0, 0.0, 1.0)))

    val world = Matrix43.translate(0.0, 0.0, 0.0) * Matrix43.rotateY(time * 0.2) * Matrix43.scale(0.01)

    sausageman.animator.modelState.worldTransform = world
    sausageman.speed = math.sin(time) * 0.5 + 1.0

    sausageman.update(0.016)

    if (viewWidth != prevWidth || viewHeight != prevHeight) {
      renderer.resizeBackbuffer(viewWidth, viewHeight)
    }

    renderer.beginFrame()

    renderer.setRenderTarget(RenderTarget.Backbuffer)
    renderer.setDepthMode(true, true)
    renderer.clear(Some(Color.rgb(0x6495ED)), Some(1.0))

    val shader = SimpleShader.get
    shader.use(p => {
      p(SimpleShader.Permutations.UseBones) = 1
    })

    renderer.pushUniform(SimpleShader.VertexGlobal, u => {
      import SimpleShader.VertexGlobal._
      ViewProjection.set(u, viewProjection)
    })

    renderer.pushUniform(SimpleShader.VertexInstance, u => {
      import SimpleShader.VertexInstance._
      World.set(u, world)
    })

    if (frameCount % 120 == 1) {
      sausageman.doPunch()
    }

    val model = sausageman.animator.model
    val modelState = sausageman.animator.modelState

    for (mesh <- model.meshes) {

      renderer.setTexture(SimpleShader.Textures.AlbedoTex, mesh.material.albedoTex.texture)
      renderer.setTexture(SimpleShader.Textures.NormalTex, mesh.material.normalTex.texture)

      for (part <- mesh.parts) {

        renderer.pushUniform(SimpleShader.VertexInstanceBones, u => {
          import SimpleShader.VertexInstanceBones._

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
      val text = LC.Test.welcome(name = "Player")
      draws += TextDraw(text, 0, text.length, Vector2(100.0, 480.0), 30.0, Color.rgb(0xFFFFFF), 0.0, 0)
    }

    val font = fontAsset.get
    font.render(draws)

    val names = ('a' to 'z').map(_.toString)
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

    renderer.endFrame()
    AppWindow.swapBuffers()

    if (mapping.justPressed(DebugInput.Reload)) {
      TestEngine.synchronized {
        processResources()
        AssetLoader.reloadEverything()
      }
    }
  }

  AppWindow.unload()

  audioEngine.closeAudio = true
  audioEngine.audioThread.join()
}
