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
import game.TestModelSystem.TestModelShader.PixelUniform
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

  val probeOffset = Vector3(0.0, 2.0, 0.0)

  for {
    y <- -5 to 5
    x <- -5 to 5
  } {
    if (x != 0 || y != 0) {
      val entity = new Entity()
      val model = ModelSystem.addModel(entity, asset)
      entity.position = Vector3(x * 8.0, 0.0, y * 8.0)

      val probe = LightSystem.addStaticProbe(entity.position + probeOffset)
      model.lightProbe = probe.probe
    }
  }

  val entity = new Entity()
  val model = ModelSystem.addModel(entity, asset)
  entity.position = Vector3(0.0, 0.0, 0.0)
  val probe = LightSystem.addStaticProbe(entity.position + probeOffset)
  model.lightProbe = probe.probe

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

  object TestModelShader extends ShaderAsset("test/instanced_mesh_light") {
    uniform(ModelSystem.InstancedUniform)
    uniform(ModelSystem.LightProbeUniform)
    uniform(GlobalUniform)

    uniform(PixelUniform)
    object PixelUniform extends UniformBlock("PixelUniform") {
      val UvBounds = vec4("UvBounds")
    }

    override val Textures = ModelTextures
  }

  TestModelShader.load()

  LightSystem.globalProbe.addDirectional(Vector3(1.0, 1.0, -1.0).normalize, Vector3(0.05, 0.05, 0.1))

  LightSystem.addStaticLight(Vector3(0.0, 30.0, 0.0), Vector3(0.4, 0.4, 0.4), 100.0)
  val light = LightSystem.addDynamicLight(Vector3.Zero, Vector3(0.8, 0.4, 0.4), 50.0)

  var renderTarget: RenderTarget = null

  val startTime = AppWindow.currentTime
  while (AppWindow.running) {
    val renderer = Renderer.get

    AppWindow.pollEvents()

    val time = AppWindow.currentTime - startTime

    radar.localTransform = Matrix43.rotateZ(time)

    light.position = Vector3(math.cos(time), 0.0, math.sin(time)) * 30.0 + Vector3(0.0, 10.0, 0.0)

    val viewWidth = AppWindow.width
    val viewHeight = AppWindow.height

    if (viewWidth != prevWidth || viewHeight != prevHeight) {
      renderer.resizeBackbuffer(viewWidth, viewHeight)

      if (renderTarget != null) renderTarget.unload()
      renderTarget = RenderTarget.create(viewWidth, viewHeight, Some("SRGB"), Some("D24S"), false, 2)
      renderTarget.setLabel("Multisample target")
    }

    val viewProjection = (
      Matrix4.perspective(viewWidth.toDouble / viewHeight.toDouble, math.Pi / 3.0, 0.01, 1000.0)
        * Matrix43.look(Vector3(0.0, 12.0, -14.0) * 4.0, Vector3(0.0, -1.0, 1.0)))

    renderer.beginFrame()
    renderer.setRenderTarget(renderTarget)
    renderer.setDepthMode(true, true)
    renderer.setWriteSrgb(true)
    renderer.clear(Some(Color.rgb(0x6495ED)), Some(1.0))

    LightSystem.addDynamicLightsToCells()
    LightSystem.evaluateProbes()
    LightSystem.finishFrame()
    ModelSystem.updateMatrices()
    ModelSystem.collectMeshInstances()
    ModelSystem.setupUniforms()
    val draws = ModelSystem.getInstancedMeshDraws()

    renderer.pushUniform(GlobalUniform, u => {
      GlobalUniform.ViewProjection.set(u, viewProjection)
    })

    renderer.setCull(true)

    val shader = TestModelShader.get
    shader.use()

    for (draw <- draws) {
      val mesh = draw.mesh
      val part = mesh.parts.head
      assert(draw.mesh.parts.length == 1)

      renderer.pushUniform(PixelUniform, u => {
        PixelUniform.UvBounds.set(u, part.uvOffsetX, part.uvOffsetY, part.uvScaleX, part.uvScaleY)
      })

      renderer.setTexture(ModelTextures.Diffuse, mesh.material.albedoTex.texture)

      renderer.bindUniform(ModelSystem.InstancedUniform, draw.instanceUbo)
      renderer.bindUniform(ModelSystem.LightProbeUniform, draw.lightProbeUbo)

      val numElems = part.numIndices
      renderer.drawElementsInstanced(draw.num, numElems, part.indexBuffer, part.vertexBuffer)
    }

    if (mapping.justPressed(DebugInput.Reload)) {
      processResources()
      AssetLoader.reloadEverything()
      ModelSystem.assetsLoaded()
    }

    renderer.blitRenderTargetColor(RenderTarget.Backbuffer, renderTarget)

    renderer.endFrame()
    AppWindow.swapBuffers()
  }

  AppWindow.unload()
}
