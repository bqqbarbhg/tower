package game

import asset._
import core._
import input._
import render._
import io.content._
import io.Toml
import main.EngineStartup
import platform.AppWindow
import res.runner.{RunOptions, Runner}

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
  }

  val pack = new MultiPackage()

  JarPackage.create("data") match {
    case Some(jar) => pack.add(jar, 1)
    case None => // Nop
  }

  pack.add(new DirectoryPackage("data"), 0)

  Package.set(pack)

  val sausageman = ModelAsset("test/sausageman/sausagemanWithTex.fbx.s2md")

  val opts = new EngineStartup.Options()
  opts.debug = true
  opts.windowName = "Engine test"
  EngineStartup.start(opts)

  val bundle = new AssetBundle(SimpleShader, sausageman)
  bundle.acquire()

  bundle.load()

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
        * Matrix43.look(Vector3(0.0, 4.0, -10.0), Vector3(0.0, 0.0, 1.0)))

    val world = Matrix43.translate(0.0, 0.0, 0.0) * Matrix43.rotateY(time * 0.5) * Matrix43.rotateX(math.Pi / 2.0)// * Matrix43.scale(0.01)

    if (viewWidth != prevWidth || viewHeight != prevHeight)
      renderer.resizeBackbuffer(viewWidth, viewHeight)

    renderer.advanceFrame()

    renderer.setRenderTarget(RenderTarget.Backbuffer)
    renderer.setDepthMode(true, true)
    renderer.clear(Some(Color.rgb(0x6495ED)), Some(1.0))

    val shader = SimpleShader.get
    shader.use()

    renderer.pushUniform(SimpleShader.VertexGlobal, u => {
      import SimpleShader.VertexGlobal._
      ViewProjection.set(u, viewProjection)
    })

    renderer.pushUniform(SimpleShader.VertexInstance, u => {
      import SimpleShader.VertexInstance._
      World.set(u, world)
    })

    val model = sausageman.get
    for (mesh <- model.meshes) {

      renderer.setTexture(SimpleShader.Textures.AlbedoTex, mesh.material.albedoTex.texture)
      renderer.setTexture(SimpleShader.Textures.NormalTex, mesh.material.normalTex.texture)

      for (part <- mesh.parts) {
        part.draw()
      }
    }

    AppWindow.swapBuffers()

    if (mapping.justPressed(DebugInput.Reload)) {
      processResources()
      AssetLoader.reloadEverything()
    }
  }

  AppWindow.unload()

}
