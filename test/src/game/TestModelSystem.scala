package game

import core._
import asset._
import game.TestEngine._
import render._
import game.system._
import io.Toml
import io.content._
import _root_.main.EngineStartup
import game.TestModelSystem.TestModelShader.PixelUniform
import CullingSystem.Viewport
import platform.AppWindow
import platform.AppWindow.WindowStyle
import res.runner.{RunOptions, Runner}
import ui.DebugDraw
import util.geometry.{Aabb, Frustum, Sphere}
import game.shader._

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

  val windowStyle = new WindowStyle(1280, 720, false, false, -1, None)
  EngineStartup.softStart(windowStyle)

  var prevWidth = -1
  var prevHeight = -1

  val asset = ModelAsset("game/tower/tower_radar.fbx.s2md")

  val probeOffset = Vector3(0.0, 2.0, 0.0)

  val viewport = new Viewport()
  viewport.viewportMask = 1

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

      if ((x + y) % 2 == 0) {
        CullingSystem.addStaticAABB(entity, Aabb(Vector3(0.0, 3.0, 0.0), Vector3(3.0, 3.0, 3.0)), 1)
      } else {
        CullingSystem.addStaticSphere(entity, Sphere(Vector3(0.0, 3.0, 0.0), 3.0), 1)
      }
    }
  }

  val entity = new Entity()
  val model = ModelSystem.addModel(entity, asset)
  entity.position = Vector3(0.0, 0.0, 0.0)
  val probe = LightSystem.addStaticProbe(entity.position + probeOffset)
  model.lightProbe = probe.probe

  CullingSystem.addStaticAABB(entity, Aabb(Vector3.Zero, Vector3(3.0, 3.0, 3.0)), 1)

  val radar = model.findNode(Identifier("Radar"))

  object GlobalUniform extends UniformBlock("GlobalUniform") {
    val ViewProjection = mat4("ViewProjection")
  }

  object ModelTextures extends SamplerBlock {
    val Diffuse = sampler2D("Diffuse", Sampler.RepeatAnisotropic)
  }

  object TestModelShader extends ShaderAsset("test/instanced_mesh_light") {
    uniform(ModelSystem.InstancedUniform)
    uniform(LightProbeUniform)
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
      renderTarget = RenderTarget.create(viewWidth, viewHeight, Some(TexFormat.SrgbA), Some("D24S"), false, 2)
      renderTarget.setLabel("Multisample target")
    }

    val offX = math.sin(time) * 1.5

    val viewProjection = (
      Matrix4.perspective(viewWidth.toDouble / viewHeight.toDouble, math.Pi / 3.0, 0.01, 1000.0)
        * Matrix43.look(Vector3(0.0, 12.0, -14.0) * 4.0, Vector3(0.0, -1.0, 1.0)))

    val viewProjectionSway = (
      Matrix4.perspective(viewWidth.toDouble / viewHeight.toDouble, math.Pi / 3.0, 0.01, 1000.0)
        * Matrix43.look(Vector3(0.0, 12.0, -14.0) * 4.0, Vector3(offX, -1.0, 1.0)))

    renderer.beginFrame()
    renderer.setRenderTarget(renderTarget)
    renderer.setDepthMode(true, true)
    renderer.setWriteSrgb(true)
    renderer.clear(Some(Color.rgb(0x6495ED)), Some(1.0))

    viewport.frustum = Frustum.fromViewProjection(viewProjectionSway)

    CullingSystem.update(viewport)

    LightSystem.addDynamicLightsToCells()
    LightSystem.evaluateProbes()
    LightSystem.finishFrame()
    ModelSystem.collectVisibleModels(viewport.visibleEntities)
    ModelSystem.updateMatrices()
    ModelSystem.collectMeshInstances()
    ModelSystem.setupUniforms()
    val draws = ModelSystem.getInstancedMeshDraws()

    renderer.pushUniform(GlobalUniform, u => {
      GlobalUniform.ViewProjection.set(u, viewProjection)
    })

    CullingSystem.debugDrawQuadTree(viewport.frustum)

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
      renderer.bindUniform(LightProbeUniform, draw.lightProbeUbo)

      val numElems = part.numIndices
      renderer.drawElementsInstanced(draw.num, numElems, part.indexBuffer, part.vertexBuffer)
    }

    if (AppWindow.keyEvents.exists(e => e.down && e.key == 'R'.toInt)) {
      processResources()
      AssetLoader.reloadEverything()
      ModelSystem.assetsLoaded()
    }

    DebugDraw.render(viewProjection)

    renderer.blitRenderTargetColor(RenderTarget.Backbuffer, renderTarget)

    renderer.endFrame()
    AppWindow.swapBuffers()
  }

  EngineStartup.softStop()
  EngineStartup.stop()
}
