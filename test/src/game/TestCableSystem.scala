package game

import core._
import asset._
import render._
import game.system._
import io.Toml
import io.content._
import _root_.main.EngineStartup
import platform.AppWindow
import res.runner.{RunOptions, Runner}
import ui.{DebugDraw, SpriteBatch}
import game.lighting.LightProbe
import game.state.LoadingState
import gfx.Shader
import org.lwjgl.system.MemoryUtil
import platform.AppWindow.WindowStyle
import render.VertexSpec.Attrib
import render.VertexSpec.DataFmt._
import util.geometry.{Aabb, Frustum, Sphere}
import game.shader._
import task.debug.SchedulerDotWriter
import task.{Scheduler, Task}
import game.system.rendering
import game.system.rendering.ModelSystem.{MeshInstanceCollection, ModelInstance}
import game.system.rendering.CableRenderSystem._
import game.system.rendering.GroundSystem.GroundPlate
import render.Renderer._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn
import scala.util.Random

object TestCableSystem extends App {

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

  val pack = new MultiPackage()
  JarPackage.create("data") match {
    case Some(jar) => pack.add(jar, 1)
    case None => // Nop
  }

  pack.add(new DirectoryPackage("data"), 0)

  Package.set(pack)

  val opts = new EngineStartup.Options()
  opts.debug = true
  opts.profile = true
  opts.windowName = "Engine test"
  EngineStartup.start(opts)

  val windowStyle = new WindowStyle(1280, 720, false, false, -1, None)
  EngineStartup.softStart(windowStyle)

  rendering.loadGlobal()
  base.loadState()
  rendering.loadState()
  rendering.loadGame()

  processResources()

  var prevWidth = -1
  var prevHeight = -1

  val asset = ModelAsset("game/tower/tower_turret.fbx.s2md")

  def getCablePaths(asset: ModelAsset): Array[Array[CableNode]] = {
    val model = asset.getShallowUnsafe

    val allNodes = mutable.ArrayBuilder.make[Array[CableNode]]()

    val childrenForName = mutable.HashMap[Identifier, Vector[Identifier]]().withDefaultValue(Vector[Identifier]())
    val roots = ArrayBuffer[Identifier]()

    def getPathRecursive(head: Vector[CableNode], name: Identifier): Unit = {
      val node = model.findNodeByName(name)
      assert(node >= 0, s"Node not found for cable node '${name.toString}'")
      val children = childrenForName(name)
      val worldTransform = model.transformToRoot(node)
      val pos = worldTransform.translation
      val dir = worldTransform.forward * 2.0
      val self = CableNode(pos, dir)
      val list = head :+ self

      if (children.nonEmpty) {
        for (child <- children)
          getPathRecursive(list, child)
      } else {
        allNodes += list.toArray
      }
    }

    val start = model.findNodeByName(Identifier("Cables"))
    if (start >= 0) {
      val cableNodes = model.getChildNodes(start)
      val numberRegex = "^(.*)\\.(\\d{3})$".r

      val names = cableNodes.map(node => new Identifier(model.nodeName(node)).toString)

      for (name <- names) {
        val nameId = Identifier(name)
        name match {
          case numberRegex(prefix, numberStr) =>
            val number = numberStr.toInt
            val parent = if (number > 1) {
              val parentNumber = number - 1
              f"$prefix.${parentNumber}%03d"
            } else {
              prefix
            }

            val parentId = Identifier(parent)
            val prev = childrenForName(parentId)
            childrenForName(parentId) = prev :+ nameId
          case _ =>

            names.find(parent => name.startsWith(parent) && name != parent) match {
              case Some(parentName) =>
                val parentId = Identifier(parentName)
                val prev = childrenForName(parentId)
                childrenForName(parentId) = prev :+ nameId
              case None =>
                roots += Identifier(name)
            }
        }
      }

      for (root <- roots) {
        getPathRecursive(Vector[CableNode](), root)
      }
    }

    allNodes.result()
  }

  /*
  val albedo = TextureAsset("test/rustediron/rustediron2_basecolor.png.s2tx")
  val normal = TextureAsset("test/rustediron/rustediron2_normal.png.s2tx")
  val roughness = TextureAsset("test/rustediron/rustediron2_roughness.png.s2tx")
  val metallic = TextureAsset("test/rustediron/rustediron2_metallic.png.s2tx")
  */

  val albedo = TextureAsset("game/tower/tower_turret_albedo.png.s2tx")
  val normal = TextureAsset("game/tower/tower_turret_normal.png.s2tx")
  val roughness = TextureAsset("game/tower/tower_turret_roughness.png.s2tx")
  val metallic = TextureAsset("game/tower/tower_turret_metallic.png.s2tx")
  val ao = TextureAsset("game/tower/tower_turret_ao.png.s2tx")

  val ground_albedo = TextureAsset("game/ground/sand_stone_albedo.png.s2tx")
  val ground_normal = TextureAsset("game/ground/sand_stone_normal.png.s2tx")
  val ground_roughness = TextureAsset("game/ground/sand_stone_roughness.png.s2tx")
  val ground_metallic = TextureAsset("game/ground/sand_stone_metallic.png.s2tx")
  val ground_ao = TextureAsset("game/ground/sand_stone_ao.png.s2tx")


  val fontAsset = FontAsset("font/open-sans/OpenSans-Regular.ttf.s2ft")

  object GlobalUniform extends UniformBlock("GlobalUniform") {
    val ViewProjection = mat4("ViewProjection")
    val ShadowViewProjection = mat4("ShadowViewProjection")
    val ViewPosition = vec4("ViewPosition")
  }

  object ModelTextures extends SamplerBlock {
    val Albedo = sampler2D("Albedo", Sampler.RepeatAnisotropic)
    val Normal = sampler2D("Normal", Sampler.RepeatAnisotropic)
    val Roughness = sampler2D("Roughness", Sampler.RepeatAnisotropic)
    val Metallic = sampler2D("Metallic", Sampler.RepeatAnisotropic)
    val AoTex = sampler2D("AoTex", Sampler.RepeatAnisotropic)
    val ShadowMap = sampler2D("ShadowMap", Sampler.ClampNearestNoMip)
  }

  object TestModelShader extends ShaderAsset("test/instanced_pbr") {

    override object Permutations extends Shader.Permutations {
      val BrdfFunc_D = frag("BrdfFunc_D", 1 to 2)
      val BrdfFunc_V = frag("BrdfFunc_V", 1 to 5)
      val BrdfFunc_F = frag("BrdfFunc_F", 1 to 2)
      val PerSampleShading = both("PerSampleShading", 0 to 1)
    }

    uniform(ModelInstanceUniform)
    uniform(LightProbeUniform)
    uniform(GlobalUniform)

    uniform(PixelUniform)
    object PixelUniform extends UniformBlock("PixelUniform") {
      val UvBounds = vec4("UvBounds")
    }

    override val Textures = ModelTextures
  }

  object TestGroundShader extends ShaderAsset("test/ground_pbr") {

    override object Permutations extends Shader.Permutations {
      val BrdfFunc_D = frag("BrdfFunc_D", 1 to 2)
      val BrdfFunc_V = frag("BrdfFunc_V", 1 to 5)
      val BrdfFunc_F = frag("BrdfFunc_F", 1 to 2)
    }

    uniform(LightProbeUniform)
    uniform(GlobalUniform)
    uniform(GroundPlateUniform)

    override object Textures extends SamplerBlock {
      val Albedo = sampler2D("Albedo", Sampler.RepeatAnisotropic)
      val Normal = sampler2D("Normal", Sampler.RepeatAnisotropic)
      val Roughness = sampler2D("Roughness", Sampler.RepeatAnisotropic)
      val Metallic = sampler2D("Metallic", Sampler.RepeatAnisotropic)
      val AoTex = sampler2D("AoTex", Sampler.RepeatAnisotropic)
      val ShadowMap = sampler2D("ShadowMap", Sampler.ClampNearestNoMip)
      val AmbientBendTex = sampler2D("AmbientBendTex", Sampler.ClampBilinearNoMip)
    }
  }

  object TestShadowShader extends ShaderAsset("test/instanced_shadow") {
    uniform(ShadowInstanceUniform)

    uniform(ShadowUniform)
    object ShadowUniform extends UniformBlock("ShadowUniform") {
      val ViewProjection = mat4("ViewProjection")
    }
  }

  object TestCableShader extends ShaderAsset("test/cable_test") {
    uniform(LightProbeUniform)
    uniform(GlobalUniform)

    uniform(CableUniform)
    object CableUniform extends UniformBlock("CableUniform") {
      val Pulse = vec4("Pulse")
    }

    override object Textures extends SamplerBlock {
      val ShadowMap = sampler2D("ShadowMap", Sampler.ClampNearestNoMip)
    }
  }

  object UpsampleSimpleShader extends ShaderAsset("shader/simple_upsample") {

    override object Textures extends SamplerBlock {
      val Multisample = sampler2D("Multisample", Sampler.ClampBilinearNoMip)
    }
  }

  object UpsampleMsaaShader extends ShaderAsset("shader/msaa_upsample") {

    uniform(VertexUniform)
    object VertexUniform extends UniformBlock("VertexUniform") {
      val TextureScale = vec4("TextureScale")
    }

    override object Permutations extends Shader.Permutations {
      val SampleCount = frag("SampleCount", Array(2, 4, 8, 16))
    }

    override object Textures extends SamplerBlock {
      val Multisample = sampler2DMS("Multisample")
      val SubsampleWeights = sampler2DArray("SubsampleWeights", Sampler.ClampBilinearNoMip)
    }
  }

  object SsaoGenShader extends ShaderAsset("shader/post/ssao_gen") {

    val NumSamples = 16

    uniform(SsaoUniform)
    object SsaoUniform extends UniformBlock("SsaoUniform") {
      val TextureScale = vec4("TextureScale")
      val DepthPlanes = vec4("DepthPlanes")
      val Projection = mat4("Projection")
      val InvProjection = mat4("InvProjection")
      val Samples = vec4("Samples", NumSamples)
    }

    override object Textures extends SamplerBlock {
      val Depth = sampler2DMS("Depth")
    }

  }

  object SsaoBlurShader extends ShaderAsset("shader/post/ssao_blur") {

    uniform(PixelUniform)
    object PixelUniform extends UniformBlock("PixelUniform") {
      val TexelSize = vec4("TexelSize")
    }

    override object Textures extends SamplerBlock {
      val Ssao = sampler2D("Ssao", Sampler.ClampBilinearNoMip)
    }
  }

  object SsaoAddShader extends ShaderAsset("shader/post/ssao_add") {

    override object Textures extends SamplerBlock {
      val Ssao = sampler2D("Ssao", Sampler.ClampBilinearNoMip)
    }
  }

  val bundle = new AssetBundle(
    "TestBundle",
    asset,
    albedo, normal, roughness, metallic, ao,
    ground_albedo, ground_normal, ground_roughness, ground_metallic, ground_ao,
    fontAsset,
    TestGroundShader,
    TestModelShader,
    TestShadowShader,
    TestCableShader,
  )

  bundle.acquire()
  bundle.load()


  val entity = new Entity(true, "Turret")
  val model = rendering.modelSystem.addModel(entity, asset)
  entity.position = Vector3(0.0, 0.0, 0.0)

  rendering.cullingSystem.addSphere(entity, Sphere(Vector3(0.0, 1.0, 0.0), 4.0), rendering.CullingSystem.MaskRender | rendering.CullingSystem.MaskShadow)

  val probe = rendering.ambientSystem.addProbe(entity, Vector3(0.0, 0.5, 0.0))
  model.lightProbe = probe.irradianceProbe

  val lightEntity = new Entity(true, "Lights")
  rendering.ambientPointLightSystem.addLight(lightEntity, Vector3(40.0, 50.0, 20.0), Vector3(1.0, 0.0, 0.0) * 3.7, 150.0)
  rendering.ambientPointLightSystem.addLight(lightEntity, Vector3(100.0, 80.0, 20.0), Vector3(0.6, 0.5, 0.5) * 2.7, 100.0)
  rendering.ambientPointLightSystem.addLight(lightEntity, Vector3(-20.0, 50.0, -20.0), Vector3(0.5, 0.5, 0.8) * 3.4, 150.0)

  val head = model.findNode(Identifier("Head")).get
  val barrel = model.findNode(Identifier("Barrel")).get

  val ambientAmount = 1.0
  val groundColor = Color.rgb(0x8a857f)
  val groundIntensity = Vector3(groundColor.r, groundColor.g, groundColor.b) * ambientAmount

  /*
  LightSystem.globalProbe.addDirectional(Vector3(0.0, +1.0, 0.0), Vector3(0.1, 0.2, 0.3) * 0.4 * ambientAmount)
  LightSystem.globalProbe.addDirectional(Vector3(0.0, -1.0, 0.0), groundIntensity * 0.25 * 0.5)
  LightSystem.globalProbe.addDirectional(Vector3(+1.0, 0.0, 0.0), groundIntensity * 0.125 * 0.5)
  LightSystem.globalProbe.addDirectional(Vector3(-1.0, 0.0, 0.0), groundIntensity * 0.125 * 0.5)
  LightSystem.globalProbe.addDirectional(Vector3(0.0, 0.0, +1.0), groundIntensity * 0.125 * 0.5)
  LightSystem.globalProbe.addDirectional(Vector3(0.0, 0.0, -1.0), groundIntensity * 0.125 * 0.5)
  */

  val GroundSpec = VertexSpec(Vector(
    Attrib(3, F32, Identifier("Position")),
    Attrib(3, F32, Identifier("Normal")),
    Attrib(4, UI8, Identifier("ProbeIndex")),
    Attrib(4, UN8, Identifier("ProbeWeight")),
  ))


  AssetLoader.preloadAtlases()

  val QuadSpec = VertexSpec(Vector(
    Attrib(2, F32, Identifier("Position"))
  ))

  val quadVerts = withStack {
    val buffer = alloca(QuadSpec.sizeInBytes * 4)
    buffer.putFloat(0.0f)
    buffer.putFloat(0.0f)
    buffer.putFloat(1.0f)
    buffer.putFloat(0.0f)
    buffer.putFloat(0.0f)
    buffer.putFloat(1.0f)
    buffer.putFloat(1.0f)
    buffer.putFloat(1.0f)
    buffer.finish()
    VertexBuffer.createStatic(QuadSpec, buffer)
  }

  val quadIndices = withStack {
    val buffer = alloca(2 * 6)
    buffer.putShort(2)
    buffer.putShort(1)
    buffer.putShort(0)
    buffer.putShort(1)
    buffer.putShort(2)
    buffer.putShort(3)
    buffer.finish()
    IndexBuffer.createStatic(buffer)
  }

  TestModelShader.load()
  TestShadowShader.load()

  val shadowTarget = RenderTarget.create(2048, 2048, None, Some(TexFormat.D24S8), true)
    .withLabel("Shadow Target")

  val normalBendTarget = RenderTarget.create(2048, 2048, Some(TexFormat.Rgba), None, false)

  val spriteBatch = new SpriteBatch()
  val turretSpriteN = Identifier("game/ao-sprite/turret_n.png")
  val turretSpriteE = Identifier("game/ao-sprite/turret_e.png")
  val turretSpriteS = Identifier("game/ao-sprite/turret_s.png")
  val turretSpriteW = Identifier("game/ao-sprite/turret_w.png")

  var renderTarget: RenderTarget = null
  var resolveTarget: RenderTarget = null
  var ssaoTarget: RenderTarget = null
  var ssaoBlurTarget: RenderTarget = null
  var angle = 0.0

  var subsampleTexture: TextureHandle = null
  var numSamples: Int = 0

  var toggle = true
  var resToggle = false
  var zoom = 0.5

  var frozenCulling: Boolean = false

  var frustum: Frustum = null

  {
    val cables = getCablePaths(asset)
    val cable = cables.head.toBuffer
    cable += new CableNode(Vector3(4.0, 0.2, 5.0), Vector3(5.0, 0.0, 0.0))
    cable += new CableNode(Vector3(8.0, 0.2, 3.0), Vector3(2.0, 0.0, -3.0))
    cable += new CableNode(Vector3(12.0, 0.2, 1.0), Vector3(10.0, 0.0, 0.0))
    cable += new CableNode(Vector3(40.0, 0.2, 5.0), Vector3(30.0, 0.0, 0.0))

    val cableEntity = new Entity(true, "Cable")
    rendering.cableRenderSystem.createCable(cableEntity, cable, 0.2)
  }

  val random = new Random()
  val ssaoSamples = Array.fill(SsaoGenShader.NumSamples)({
    val vec = Vector3(random.nextDouble() * 2.0 - 1.0, random.nextDouble() * 2.0 - 1.0, random.nextDouble() + 0.05)
    vec.normalize * (random.nextDouble() + 0.05)
  })

  val startTime = AppWindow.currentTime
  var prevTime = startTime
  while (AppWindow.running) {
    val renderer = Renderer.get

    val keyEvents = AppWindow.keyEvents
    def justPressed(key: Int) = keyEvents.exists(e => e.down && e.key == key)
    def isDown(key: Int) = AppWindow.keyDown(key)

    if (justPressed('T'.toInt))
      toggle = !toggle

    if (justPressed('Y'.toInt))
      resToggle = !resToggle

    AppWindow.pollEvents()

    val time = AppWindow.currentTime - startTime
    val dt = time - prevTime
    prevTime = time

    val viewWidth = AppWindow.width
    val viewHeight = AppWindow.height

    var width = viewWidth
    var height = viewHeight

    if (resToggle) {
      width = width * 1 / 2
      height = height * 1 / 2
    }

    if (renderTarget == null || width != renderTarget.width || height != renderTarget.height) {
      renderer.resizeBackbuffer(viewWidth, viewHeight)

      if (renderTarget != null) renderTarget.unload()
      if (resolveTarget != null) resolveTarget.unload()
      renderTarget = RenderTarget.create(width, height, Some(TexFormat.SrgbA), Some("D24S"), true, 4)
      resolveTarget = RenderTarget.create(width, height, Some(TexFormat.SrgbA), None, false)
      renderTarget.setLabel("Multisample target")

      ssaoTarget = RenderTarget.create(width / 2, height / 2, Some(TexFormat.Rgba), None, false)
      ssaoBlurTarget = RenderTarget.create(width / 2, height / 2, Some(TexFormat.Rgba), None, false)

      val samples = renderTarget.sampleLocations
      numSamples = samples.length

      val subRes = 32

      if (subsampleTexture != null) subsampleTexture.free()
      subsampleTexture = TextureHandle.createArray(subRes, subRes, "RA16", numSamples, 1, false)
      subsampleTexture.setLabel("Subsample lookup")

      val sampleWeights = Array.fill(numSamples, subRes * subRes * 4)(0.0)

      for {
        y <- 0 until subRes
        x <- 0 until subRes
      } {
        val p = Vector2(x.toDouble, y.toDouble) / (subRes - 1).toDouble + Vector2(0.5, 0.5)
        val offset = (y * subRes + x) * 4

        for ((sample, sampleIx) <- samples.zipWithIndex) {
          val weights = sampleWeights(sampleIx)

          val s00 = sample + Vector2(0.0, 0.0)
          val s10 = sample + Vector2(1.0, 0.0)
          val s01 = sample + Vector2(0.0, 1.0)
          val s11 = sample + Vector2(1.0, 1.0)

          weights(offset + 0) = math.pow(math.max(0.8 - (p - s00).length, 0.0), 2.0)
          weights(offset + 1) = math.pow(math.max(0.8 - (p - s10).length, 0.0), 2.0)
          weights(offset + 2) = math.pow(math.max(0.8 - (p - s01).length, 0.0), 2.0)
          weights(offset + 3) = math.pow(math.max(0.8 - (p - s11).length, 0.0), 2.0)
        }
      }

      for {
        y <- 0 until subRes
        x <- 0 until subRes
      } {
        var total = 0.0
        val offset = (y * subRes + x) * 4
        for (sampleIx <- 0 until numSamples) {
          val weights = sampleWeights(sampleIx)
          total += weights(offset + 0)
          total += weights(offset + 1)
          total += weights(offset + 2)
          total += weights(offset + 3)
        }

        for (sampleIx <- 0 until numSamples) {
          val weights = sampleWeights(sampleIx)
          weights(offset + 0) /= total
          weights(offset + 1) /= total
          weights(offset + 2) /= total
          weights(offset + 3) /= total
        }
      }

      val buf = MemoryUtil.memAlloc(subRes * subRes * 4 * 2)
      for ((weights, sampleIx) <- sampleWeights.zipWithIndex) {
        for (w <- weights) {
          buf.putShort(math.min((w * 65535.0).toInt, 65535).toShort)
        }
        buf.finish()
        subsampleTexture.setLayerData(sampleIx, subRes, subRes, "RA16", Array(buf))
      }
      MemoryUtil.memFree(buf)
    }

    if (isDown('S'.toInt)) zoom += dt * 2.0
    if (isDown('W'.toInt)) zoom -= dt * 2.0
    zoom = clamp(zoom, 0.5, 1.5)

    val proj = Matrix4.perspective(viewWidth.toDouble / viewHeight.toDouble, math.Pi / 3.0, 1.0, 1000.0)
    val viewPos = Vector3(math.sin(angle) * 18.0, 30.0, math.cos(angle) * -18.0) * zoom
    val viewProjection = proj * Matrix43.look(viewPos, Vector3(-math.sin(angle), -1.5, math.cos(angle)))

    val shadowViewProjection = (
      Matrix4.orthographic(80.0, 80.0, 0.1, 100.0)
        * Matrix43.look(Vector3(0.25, 0.75, -0.25) * 40.0, -Vector3(0.25, 0.75, -0.25)))

    if (AppWindow.keyEvents.exists(e => e.down && e.key == 'F'))
      frozenCulling = !frozenCulling

    if (!frozenCulling)
      frustum = Frustum.fromViewProjection(viewProjection)

    val tt = time
    head.localTransform = Matrix43.rotateZ(math.sin(tt * 0.6) * 0.5) * Matrix43.rotateX(math.sin(tt * 0.5) * 0.2)
    barrel.localTransform = Matrix43.rotateY(tt * -15.0)

    val debugger = new SchedulerDotWriter()

    val s = new Scheduler()
    s.attachDebugger(debugger)

    var visProbes: ArrayBuffer[rendering.AmbientSystem.Probe] = null
    var visMeshes: MeshInstanceCollection = null
    var shadowMeshes: MeshInstanceCollection = null
    var forwardDraws: rendering.ForwardRenderingSystem.Draws = null
    var shadowDraws: rendering.ShadowRenderingSystem.Draws = null
    var visCables: ArrayBuffer[CableMeshPart] = null
    var visGround: ArrayBuffer[GroundPlate] = null

    val visEntities = new EntitySet()
    val shadowEntities = new EntitySet()

    object Vis
    object VisModel
    object VisMesh
    object VisProbes
    object VisCables
    object VisGround

    object Shadow
    object ShadowModel
    object ShadowMesh

    val shadowFrustum = Frustum.fromViewProjection(shadowViewProjection)

    s.add("Viewport cull")(rendering.cullingSystem, Vis)() {
      rendering.cullingSystem.cullEntities(visEntities, frustum, rendering.CullingSystem.MaskRender)
    }

    s.add("Shadow cull")(rendering.cullingSystem, Shadow)() {
      rendering.cullingSystem.cullEntities(shadowEntities, shadowFrustum, rendering.CullingSystem.MaskShadow)
    }

    s.add("Probe collect")(rendering.ambientSystem, VisProbes)(Vis) {
      visProbes = rendering.ambientSystem.updateVisibleProbes(visEntities)
    }

    s.add("Ambient point dynamic")(rendering.ambientPointLightSystem)() {
      rendering.ambientPointLightSystem.updateDynamicLights()
    }

    s.add("Ambient probe points")(VisProbes)(rendering.ambientPointLightSystem) {
      rendering.ambientPointLightSystem.updateVisibleProbes(visProbes)
    }

    s.add("Ambient probe indirect")(rendering.ambientSystem, VisProbes)() {
      rendering.ambientSystem.updateIndirectLight(visProbes)
    }

    s.add("Model update")(rendering.modelSystem, VisMesh)(Vis) {
      val models = rendering.modelSystem.collectVisibleModels(visEntities)
      rendering.modelSystem.updateModels(models)
      visMeshes = rendering.modelSystem.collectMeshInstances(models)
    }

    s.add("Shadow model update")(rendering.modelSystem, ShadowMesh)(Shadow) {
      val models = rendering.modelSystem.collectVisibleModels(shadowEntities)
      rendering.modelSystem.updateModels(models)
      shadowMeshes = rendering.modelSystem.collectMeshInstances(models)
    }

    s.add("Collect cables")(rendering.cableRenderSystem, VisCables)(Vis) {
      visCables = rendering.cableRenderSystem.collectCableMeshes(visEntities)
    }

    s.add("Collect ground")(rendering.groundSystem, VisGround)(Vis) {
      visGround = rendering.groundSystem.collectGroundPlates(visEntities)
    }

    s.addTo("Forward draws")(Task.Main)(rendering.forwardRenderingSystem)(VisMesh, VisProbes) {
      forwardDraws = rendering.forwardRenderingSystem.createMeshDraws(visMeshes)
    }

    s.addTo("Shadow draws")(Task.Main)(rendering.shadowRenderingSystem)(ShadowMesh) {
      shadowDraws = rendering.shadowRenderingSystem.createMeshDraws(shadowMeshes)
    }

    s.add("Ambient point cleanup")(rendering.ambientPointLightSystem)() {
      rendering.ambientPointLightSystem.frameCleanup()
    }

    s.add("Ambient cleanup")(rendering.ambientSystem, visProbes)() {
      rendering.ambientSystem.frameCleanup(visProbes)
    }

    s.add("Model cleanup")(rendering.modelSystem)() {
      rendering.modelSystem.frameCleanup()
    }

    s.finish()

    if (AppWindow.keyEvents.exists(e => e.down && e.key == 'P')) {
      println(debugger.result)
    }

    renderer.beginFrame()

    renderer.setRenderTarget(normalBendTarget)
    renderer.setDepth(DepthNone)
    renderer.setWriteSrgb(false)
    renderer.clear(Some(Color(0.0, 0.0, 0.0, 0.0)), None)

    val colorN = Color(1.0, 0.0, 0.0, 0.0)
    val colorE = Color(0.0, 1.0, 0.0, 0.0)
    val colorS = Color(0.0, 0.0, 1.0, 0.0)
    val colorW = Color(0.0, 0.0, 0.0, 1.0)

    renderer.setBlend(Renderer.BlendAddAlpha)
    spriteBatch.draw(turretSpriteN, Vector2(50.0, 50.0), Vector2(100.0, 100.0), colorN)
    spriteBatch.draw(turretSpriteE, Vector2(50.0, 50.0), Vector2(100.0, 100.0), colorE)
    spriteBatch.draw(turretSpriteS, Vector2(50.0, 50.0), Vector2(100.0, 100.0), colorS)
    spriteBatch.draw(turretSpriteW, Vector2(50.0, 50.0), Vector2(100.0, 100.0), colorW)

    spriteBatch.flush()

    renderer.setRenderTarget(shadowTarget)
    renderer.setDepth(DepthWrite)
    renderer.setWriteSrgb(false)
    renderer.clear(None, Some(1.0))

    renderer.setBlend(Renderer.BlendNone)

    renderer.pushUniform(TestShadowShader.ShadowUniform, u => {
      TestShadowShader.ShadowUniform.ViewProjection.set(u, shadowViewProjection)
    })

    TestShadowShader.get.use()

    for (draw <- shadowDraws.instanced) {
      val part = draw.mesh
      val mesh = part.mesh

      renderer.bindUniform(ShadowInstanceUniform, draw.instanceUbo)

      val numElems = part.numIndices
      renderer.drawElementsInstanced(draw.num, numElems, part.indexBuffer, part.vertexBuffer)
    }

    renderer.setRenderTarget(renderTarget)
    renderer.setDepth(DepthWrite)
    renderer.setWriteSrgb(true)
    renderer.clear(Some(Color.rgb(0x6495ED)), Some(1.0))

    renderer.pushUniform(GlobalUniform, u => {
      GlobalUniform.ViewProjection.set(u, viewProjection)
      GlobalUniform.ShadowViewProjection.set(u, shadowViewProjection)
      GlobalUniform.ViewPosition.set(u, viewPos, 0.0f)
    })

    renderer.setCull(CullNormal)

    val shader = TestModelShader.get
    shader.use(p => {
      if (true) {
        p(TestModelShader.Permutations.BrdfFunc_D) = 2
        p(TestModelShader.Permutations.BrdfFunc_V) = 2
        p(TestModelShader.Permutations.BrdfFunc_F) = 2
      } else {
        p(TestModelShader.Permutations.BrdfFunc_D) = 2
        p(TestModelShader.Permutations.BrdfFunc_V) = 1
        p(TestModelShader.Permutations.BrdfFunc_F) = 1
      }
      p(TestModelShader.Permutations.PerSampleShading) = resToggle
    })

    /*
    for (draw <- draws) {
      val mesh = draw.mesh
      val part = mesh.parts.head
      assert(draw.mesh.parts.length == 1)

      renderer.pushUniform(TestModelShader.PixelUniform, u => {
        TestModelShader.PixelUniform.UvBounds.set(u, part.uvOffsetX, part.uvOffsetY, part.uvScaleX, part.uvScaleY)
      })

      renderer.setTexture(ModelTextures.Albedo, albedo.get.texture)
      renderer.setTexture(ModelTextures.Normal, normal.get.texture)
      renderer.setTexture(ModelTextures.Roughness, roughness.get.texture)
      renderer.setTexture(ModelTextures.Metallic, metallic.get.texture)
      renderer.setTexture(ModelTextures.AoTex, ao.get.texture)
      renderer.setTextureTargetDepth(ModelTextures.ShadowMap, shadowTarget)

      renderer.bindUniform(ModelInstanceUniform, draw.instanceUbo)
      renderer.bindUniform(LightProbeUniform, draw.lightProbeUbo)

      val numElems = part.numIndices
      renderer.drawElementsInstanced(draw.num, numElems, part.indexBuffer, part.vertexBuffer)
    }
    */

    for (draw <- forwardDraws.instanced) {
      val part = draw.mesh
      val mesh = part.mesh

      renderer.pushUniform(TestModelShader.PixelUniform, u => {
        TestModelShader.PixelUniform.UvBounds.set(u, part.uvOffsetX, part.uvOffsetY, part.uvScaleX, part.uvScaleY)
      })

      renderer.setTexture(ModelTextures.Albedo, albedo.get.texture)
      renderer.setTexture(ModelTextures.Normal, normal.get.texture)
      renderer.setTexture(ModelTextures.Roughness, roughness.get.texture)
      renderer.setTexture(ModelTextures.Metallic, metallic.get.texture)
      renderer.setTexture(ModelTextures.AoTex, ao.get.texture)
      renderer.setTextureTargetDepth(ModelTextures.ShadowMap, shadowTarget)

      renderer.bindUniform(ModelInstanceUniform, draw.instanceUbo)
      renderer.bindUniform(LightProbeUniform, draw.lightProbeUbo)

      val numElems = part.numIndices
      renderer.drawElementsInstanced(draw.num, numElems, part.indexBuffer, part.vertexBuffer)
    }

    TestCableShader.get.use()

    renderer.pushUniform(TestCableShader.CableUniform, u => {
      TestCableShader.CableUniform.Pulse.set(u, time.toFloat, 0.0f, 0.0f, 0.0f)
    })

    renderer.setTextureTargetDepth(TestCableShader.Textures.ShadowMap, shadowTarget)

    for (cable <- visCables) {
      cable.draw()
    }

    renderer.setTexture(TestGroundShader.Textures.Albedo, ground_albedo.get.texture)
    renderer.setTexture(TestGroundShader.Textures.Normal, ground_normal.get.texture)
    renderer.setTexture(TestGroundShader.Textures.Roughness, ground_roughness.get.texture)
    renderer.setTexture(TestGroundShader.Textures.Metallic, ground_metallic.get.texture)
    renderer.setTexture(TestGroundShader.Textures.AoTex, ground_ao.get.texture)
    renderer.setTextureTargetColor(TestGroundShader.Textures.AmbientBendTex, normalBendTarget, 0)
    TestGroundShader.get.use(p => {
      if (true) {
        p(TestGroundShader.Permutations.BrdfFunc_D) = 2
        p(TestGroundShader.Permutations.BrdfFunc_V) = 2
        p(TestGroundShader.Permutations.BrdfFunc_F) = 2
      } else {
        p(TestGroundShader.Permutations.BrdfFunc_D) = 2
        p(TestGroundShader.Permutations.BrdfFunc_V) = 1
        p(TestGroundShader.Permutations.BrdfFunc_F) = 1
      }
    })

    for (plate <- visGround) {
      plate.draw()
    }

    if (justPressed('R'.toInt)) {
      processResources()
      AssetLoader.reloadEverything()
      rendering.modelSystem.assetsLoaded()
    }

    if (isDown('A'.toInt)) {
      angle += dt * 2.0
    }
    if (isDown('D'.toInt)) {
      angle -= dt * 2.0
    }

    val cables = getCablePaths(asset)

    /*
    var totalPoints = 0
    if (toggle) {
    for (cable <- cables) {
      val points = CableRenderSystem.createCablePoints(cable)
      DebugDraw.drawConnectedLine(points, Color.rgb(0x000000))
      if (false) {
        for (point <- points) {
          DebugDraw.drawLine(point - Vector3(0.1, 0.0, 0.0), point + Vector3(0.1, 0.0, 0.0), Color.rgb(0xFF0000))
          DebugDraw.drawLine(point - Vector3(0.0, 0.1, 0.0), point + Vector3(0.0, 0.1, 0.0), Color.rgb(0xFF0000))
          DebugDraw.drawLine(point - Vector3(0.0, 0.0, 0.1), point + Vector3(0.0, 0.0, 0.1), Color.rgb(0xFF0000))
        }
      }
      totalPoints += points.length
    }
    } else {
    for (cable <- cables) {
      for (Array(prev, next) <- cable.sliding(2)) {
        val Steps = 6

        val p0 = prev.position
        val m0 = prev.tangent
        val p1 = next.position
        val m1 = next.tangent

        val verts = Vector.tabulate(Steps)(index => {
          val t = index.toDouble / (Steps - 1).toDouble
          val pos = Hermite.interpolate(p0, m0, p1, m1, t)
          (pos, Color.rgb(0x000000))
        })

        DebugDraw.drawConnectedLine(verts)
        totalPoints += verts.length

        if (false) {
        for (vert <- verts) {
          val point = vert._1
          DebugDraw.drawLine(point - Vector3(0.1, 0.0, 0.0), point + Vector3(0.1, 0.0, 0.0), Color.rgb(0xFF0000))
          DebugDraw.drawLine(point - Vector3(0.0, 0.1, 0.0), point + Vector3(0.0, 0.1, 0.0), Color.rgb(0xFF0000))
          DebugDraw.drawLine(point - Vector3(0.0, 0.0, 0.1), point + Vector3(0.0, 0.0, 0.1), Color.rgb(0xFF0000))
        }
        }
      }
    }
    }
    */

    /*
    for (cable <- cables) {
      var prevUp = Vector3(0.0, 1.0, 0.0)
      val positions = (for (Array(prev, next) <- cable.sliding(2)) yield {
        val Steps = 32

        val p0 = prev.position
        val m0 = prev.direction
        val p1 = next.position
        val m1 = next.direction

        Vector.tabulate(Steps)(index => {
          val t = index.toDouble / (Steps - 1).toDouble
          Hermite.interpolate(p0, m0, p1, m1, t)
        })


      }).flatten.toVector

      var tt = time * 5.0
      val offsetPositions = for (i <- positions.indices) yield {
        val cur = positions(i)
        val prev = positions.lift(i - 1).getOrElse(cur)
        val next = positions.lift(i + 1).getOrElse(cur)

        val normal = ((cur - prev) + (next - cur)) / 2.0
        val tangent = normal cross prevUp
        val up = (tangent cross normal).normalize
        prevUp = up

        tt += 1.0
        cur + (up * math.sin(tt) + tangent.normalize * math.cos(tt)) * 0.05
      }
      DebugDraw.drawConnectedLine(offsetPositions, Color.rgb(0xFF0000))
    }
    */

    DebugDraw.render(viewProjection)

    renderer.setBlend(Renderer.BlendNone)
    renderer.setWriteSrgb(false)
    renderer.setRenderTarget(ssaoTarget)

    renderer.pushUniform(SsaoGenShader.SsaoUniform, u => {
      val m33 = proj.m33.toFloat
      val m34 = proj.m34.toFloat

      SsaoGenShader.SsaoUniform.TextureScale.set(u, width.toFloat, height.toFloat, 0.0f, 0.0f)
      SsaoGenShader.SsaoUniform.DepthPlanes.set(u, m33, m34, 0.0f, 0.0f)
      SsaoGenShader.SsaoUniform.Projection.set(u, proj)
      SsaoGenShader.SsaoUniform.InvProjection.set(u, proj.inverse)
      for ((s, i) <- ssaoSamples.zipWithIndex) {
        SsaoGenShader.SsaoUniform.Samples.set(u, i, s, 0.0f)
      }
    })
    renderer.setTextureTargetDepth(SsaoGenShader.Textures.Depth, renderTarget)
    SsaoGenShader.get.use()
    renderer.drawQuad()

    renderer.setRenderTarget(ssaoBlurTarget)

    renderer.pushUniform(SsaoBlurShader.PixelUniform, u => {
      SsaoBlurShader.PixelUniform.TexelSize.set(u, 1.0f / ssaoTarget.width.toFloat, 1.0f / ssaoTarget.height.toFloat, 0.0f, 0.0f)
    })
    renderer.setTextureTargetColor(SsaoBlurShader.Textures.Ssao, ssaoTarget, 0)
    SsaoBlurShader.get.use()
    renderer.drawQuad()

    renderer.setRenderTarget(RenderTarget.Backbuffer)

    renderer.clear(Some(Color.Black), Some(1.0))
    renderer.setBlend(Renderer.BlendNone)
    renderer.setDepth(DepthNone)
    renderer.setCull(CullNone)

    if (resToggle) {
      renderer.blitRenderTargetColor(resolveTarget, renderTarget)
      if (!toggle) {
        UpsampleSimpleShader.get.use()
        renderer.setTextureTargetColor(UpsampleSimpleShader.Textures.Multisample, resolveTarget, 0)
        renderer.drawElements(6, quadIndices, quadVerts)
      } else {
        UpsampleMsaaShader.get.use(p => {
          p(UpsampleMsaaShader.Permutations.SampleCount) = numSamples
        })
        renderer.pushUniform(UpsampleMsaaShader.VertexUniform, u => {
          UpsampleMsaaShader.VertexUniform.TextureScale.set(u, renderTarget.width.toFloat, renderTarget.height.toFloat, 0.0f, 0.0f)
        })
        renderer.setTextureTargetColor(UpsampleMsaaShader.Textures.Multisample, renderTarget, 0)
        renderer.setTexture(UpsampleMsaaShader.Textures.SubsampleWeights, subsampleTexture)
        renderer.drawElements(6, quadIndices, quadVerts)
      }
    } else {
      renderer.blitRenderTargetColor(RenderTarget.Backbuffer, renderTarget)
      renderer.setRenderTarget(RenderTarget.Backbuffer)
    }

    renderer.setBlend(Renderer.BlendMultiply)
    renderer.setTextureTargetColor(SsaoAddShader.Textures.Ssao, ssaoBlurTarget, 0)
    SsaoAddShader.get.use()
    renderer.drawQuad()

    renderer.setBlend(Renderer.BlendAlpha)
    renderer.setDepth(DepthNone)

    {
      val draws = ArrayBuffer[ui.Font.TextDraw]()

      {
        val text = s"Toggle: $toggle"
        draws += ui.Font.TextDraw(text, 0, text.length, Vector2(100.0, 134.0), 22.0, Color.rgb(0xFFFFFF), 0.0, 1)
      }

      {
        val time = renderer.averageFrameTime
        val text = f"GPU time: $time%.2fms"
        draws += ui.Font.TextDraw(text, 0, text.length, Vector2(100.0, 174.0), 22.0, Color.rgb(0xFFFFFF), 0.0, 1)
      }

      fontAsset.get.render(draws)
    }

    renderer.endFrame()

    AppWindow.swapBuffers()
  }

  EngineStartup.softStop()
  EngineStartup.stop()
}

