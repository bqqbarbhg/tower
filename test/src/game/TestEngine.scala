package game

import asset._
import core._
import game.test.Sausageman
import gfx.{ModelState, Shader}
import input._
import render._
import io.content._
import io.Toml
import main.EngineStartup
import platform.AppWindow
import res.runner.{RunOptions, Runner}
import ui.Font.TextDraw

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

  val sausagemanAsset = ModelAsset("test/sausageman/sausagemanWithTex.fbx.s2md")
  val fontAsset = FontAsset("font/open-sans/OpenSans-Regular.ttf.s2ft")

  val opts = new EngineStartup.Options()
  opts.debug = true
  opts.windowName = "Engine test"
  EngineStartup.start(opts)

  val bundle = new AssetBundle(SimpleShader, sausagemanAsset)
  bundle.acquire()

  bundle.load()

  val sausageman = new Sausageman(sausagemanAsset)

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

    if (viewWidth != prevWidth || viewHeight != prevHeight)
      renderer.resizeBackbuffer(viewWidth, viewHeight)

    renderer.advanceFrame()

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

    val font = fontAsset.get
    font.render(draws)

    AppWindow.swapBuffers()

    if (mapping.justPressed(DebugInput.Reload)) {
      processResources()
      AssetLoader.reloadEverything()
    }
  }

  AppWindow.unload()

}
