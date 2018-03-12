package game

import core._
import asset._
import game.TestEngine.DebugInput.button
import game.TestEngine._
import render._
import game.system._
import input.{InputMapping, InputSet}
import io.Toml
import io.content._
import _root_.main.EngineStartup
import platform.AppWindow
import res.runner.{RunOptions, Runner}

object TestModelSystem extends App {

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

  val pack = new MultiPackage()

  JarPackage.create("data") match {
    case Some(jar) => pack.add(jar, 1)
    case None => // Nop
  }

  pack.add(new DirectoryPackage("data"), 0)

  Package.set(pack)

  val opts = new EngineStartup.Options()
  opts.debug = true
  opts.windowName = "Engine test"
  EngineStartup.start(opts)

  var prevWidth = -1
  var prevHeight = -1

  val asset = ModelAsset("game/tower/tower_radar.fbx.s2md")

  for {
    y <- -5 to 5
    x <- -5 to 5
  }
  {
    if (x != 0 || y != 0) {
      val entity = new Entity()
      val model = ModelSystem.addModel(entity, asset)
      model.transform = Matrix43.scale(0.01)
      entity.position = Vector3(x * 8.0, 0.0, y * 8.0)
    }
  }

  val entity = new Entity()
  val model = ModelSystem.addModel(entity, asset)
  model.transform = Matrix43.scale(0.01)
  entity.position = Vector3(0.0, 0.0, 0.0)

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

  val radar = model.findNode(Identifier("Radar"))

  object GlobalUniform extends UniformBlock("GlobalUniform") {
    val ViewProjection = mat4("ViewProjection")
  }

  object ModelTextures extends SamplerBlock {
    val Diffuse = sampler2D("Diffuse", Sampler.RepeatAnisotropic)
  }

  object TestModelShader extends ShaderAsset("test/instanced_mesh") {
    uniform(ModelSystem.InstancedUniform)
    uniform(GlobalUniform)

    override val Textures = ModelTextures
  }

  val startTime = AppWindow.currentTime
  while (AppWindow.running) {
    val renderer = Renderer.get

    AppWindow.pollEvents()

    val time = AppWindow.currentTime - startTime

    radar.localTransform = Matrix43.rotateZ(time)

    val viewWidth = AppWindow.width
    val viewHeight = AppWindow.height
    if (viewWidth != prevWidth || viewHeight != prevHeight)
      renderer.resizeBackbuffer(viewWidth, viewHeight)

    val viewProjection = (
      Matrix4.perspective(viewWidth.toDouble / viewHeight.toDouble, math.Pi / 3.0, 0.01, 1000.0)
        * Matrix43.look(Vector3(0.0, 12.0, -14.0) * 4.0, Vector3(0.0, -1.0, 1.0)))

    renderer.advanceFrame()
    renderer.setRenderTarget(RenderTarget.Backbuffer)
    renderer.setDepthMode(true, true)
    renderer.clear(Some(Color.rgb(0x6495ED)), Some(1.0))

    ModelSystem.updateMatrices()
    ModelSystem.collectMeshInstances()
    ModelSystem.setupUniforms()
    val draws = ModelSystem.getInstancedMesheDraws()

    renderer.pushUniform(GlobalUniform, u => {
      GlobalUniform.ViewProjection.set(u, viewProjection)
    })

    val shader = TestModelShader.get
    shader.use()

    for (draw <- draws) {
      val mesh = draw.mesh
      val part = mesh.parts.head
      assert(draw.mesh.parts.length == 1)

      renderer.setTexture(ModelTextures.Diffuse, mesh.material.albedoTex.texture)
      renderer.bindUniform(ModelSystem.InstancedUniform, draw.ubo)

      val numElems = part.numIndices
      renderer.drawElementsInstanced(draw.num, numElems, part.indexBuffer, part.vertexBuffer)
    }

    if (mapping.justPressed(DebugInput.Reload)) {
      processResources()
      AssetLoader.reloadEverything()
      ModelSystem.assetsLoaded()
    }

    AppWindow.swapBuffers()
  }

  AppWindow.unload()
}
