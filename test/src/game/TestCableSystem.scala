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
import ui.DebugDraw
import CableRenderSystem.{CableMesh, CableNode}
import game.lighting.LightProbe
import gfx.Shader
import org.lwjgl.system.MemoryUtil
import render.VertexSpec.Attrib
import render.VertexSpec.DataFmt._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

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

  processResources()

  val pack = new MultiPackage()

  JarPackage.create("data") match {
    case Some(jar) => pack.add(jar, 1)
    case None => // Nop
  }

  pack.add(new DirectoryPackage("data"), 0)

  Package.set(pack)

  val opts = new EngineStartup.Options()
  opts.debug = false
  opts.windowName = "Engine test"
  EngineStartup.start(opts)

  var prevWidth = -1
  var prevHeight = -1

  val asset = ModelAsset("game/tower/tower_turret.fbx.s2md")

  val entity = new Entity()
  val model = ModelSystem.addModel(entity, asset)
  entity.position = Vector3(0.0, 0.0, 0.0)

  val head = model.findNode(Identifier("Head"))
  val barrel = model.findNode(Identifier("Barrel"))

  model.lightProbe = LightSystem.addStaticProbe(Vector3(0.0, 0.5, 0.0)).probe
  // LightSystem.addDynamicLight(Vector3(40.0, 20.0, 20.0), Vector3(1.0, 0.0, 0.0) * 4.0, 150.0)
  // LightSystem.addStaticLight(Vector3(100.0, 80.0, 20.0), Vector3(0.6, 0.5, 0.5) * 1.0, 100.0)
  // LightSystem.addStaticLight(Vector3(-20.0, 20.0, -20.0), Vector3(0.5, 0.5, 0.8) * 0.6, 100.0)

  LightSystem.addStaticLight(Vector3(-10.0, 40.0, 20.0), Vector3(1.0, 0.5, 0.5) * 2.0, 60.0)
  LightSystem.addStaticLight(Vector3(40.0, 40.0, 20.0), Vector3(0.5, 0.5, 2.0) * 2.0, 60.0)


  val ambientAmount = 0.2
  val groundColor = Color.rgb(0x8a857f)
  val groundIntensity = Vector3(groundColor.r, groundColor.g, groundColor.b) * ambientAmount
  LightSystem.globalProbe.addDirectional(Vector3(0.0, +1.0, 0.0), Vector3(0.1, 0.2, 0.3) * 0.4 * ambientAmount)
  LightSystem.globalProbe.addDirectional(Vector3(0.0, -1.0, 0.0), groundIntensity * 0.25 * 0.5)
  LightSystem.globalProbe.addDirectional(Vector3(+1.0, 0.0, 0.0), groundIntensity * 0.125 * 0.5)
  LightSystem.globalProbe.addDirectional(Vector3(-1.0, 0.0, 0.0), groundIntensity * 0.125 * 0.5)
  LightSystem.globalProbe.addDirectional(Vector3(0.0, 0.0, +1.0), groundIntensity * 0.125 * 0.5)
  LightSystem.globalProbe.addDirectional(Vector3(0.0, 0.0, -1.0), groundIntensity * 0.125 * 0.5)

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

  object DebugInput extends InputSet("Debug") {
    val Reload = button("Reload")
    val Toggle = button("Toggle")
    val Left = button("Left")
    val Right = button("Right")
    val Up = button("Up")
    val Down = button("Down")
  }

  val fontAsset = FontAsset("font/open-sans/OpenSans-Regular.ttf.s2ft")

  val keyboard = AppWindow.keyboard
  val debugMapping = Toml.parse(
    """
      |[Keyboard.Debug]
      |Reload = "R"
      |Toggle = "T"
      |Left = "A"
      |Right = "D"
      |Up = "W"
      |Down = "S"
    """.stripMargin)

  val mapping = new InputMapping(Array(keyboard))
  mapping.init(debugMapping)

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
    }

    uniform(ModelSystem.InstancedUniform)
    uniform(ModelSystem.LightProbeUniform)
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

    uniform(ModelSystem.LightProbeUniform)
    uniform(GlobalUniform)

    override val Textures = ModelTextures
  }

  object TestShadowShader extends ShaderAsset("test/instanced_shadow") {
    uniform(ModelSystem.InstancedUniform)

    uniform(ShadowUniform)
    object ShadowUniform extends UniformBlock("ShadowUniform") {
      val ViewProjection = mat4("ViewProjection")
    }
  }

  object TestCableShader extends ShaderAsset("test/cable_test") {
    uniform(ModelSystem.LightProbeUniform)
    uniform(GlobalUniform)

    uniform(CableUniform)
    object CableUniform extends UniformBlock("CableUniform") {
      val Pulse = vec4("Pulse")
    }

    override object Textures extends SamplerBlock {
      val ShadowMap = sampler2D("ShadowMap", Sampler.ClampNearestNoMip)
    }
  }

  val GroundSpec = VertexSpec(Vector(
    Attrib(3, F32, Identifier("Position")),
    Attrib(3, F32, Identifier("Normal")),
    Attrib(4, UI8, Identifier("ProbeIndex")),
    Attrib(4, UN8, Identifier("ProbeWeight")),
  ))

  class GroundNode(val x: Double, val y: Double) {
    val probe = LightSystem.addStaticProbe(Vector3(x, 0.0, y))
  }

  val groundMap = new mutable.HashMap[(Int, Int), GroundNode]()

  def getGroundNode(x: Int, y: Int): GroundNode = {
    groundMap.getOrElseUpdate((x, y), {
      val xx = x.toDouble * 16.0
      val yy = y.toDouble * 16.0
      new GroundNode(xx, yy)
    })
  }

  class GroundPatch {
    var probes: Array[LightProbe] = null
    var vertices: VertexBuffer = null
  }

  def createGroundPatch(startX: Int, startY: Int, res: Int, numX: Int, numY: Int): GroundPatch = {
    val patch = new GroundPatch()
    val vertsPerChunk = (res + 1) * (res + 1)
    val vertsTotal = vertsPerChunk * numX * numY
    val groundVertSize = GroundSpec.sizeInBytes * vertsTotal
    val groundVerts = MemoryUtil.memAlloc(groundVertSize)


    patch.probes = new Array[LightProbe]((numX + 1) * (numY + 1))

    for {
      chunkY <- 0 to numY
      chunkX <- 0 to numX
    } {
      patch.probes(chunkY * (numX + 1) + chunkX) = getGroundNode(chunkX + startX, chunkY + startY).probe.probe
    }

    val invRes = 1.0 / res.toDouble

    for {
      chunkY <- 0 until numY
      chunkX <- 0 until numX
    } {
      val baseVert = (chunkY * numX + chunkX) * vertsPerChunk

      val x0 = getGroundNode(chunkX + startX, chunkY + startY).x
      val x1 = getGroundNode(chunkX + startX + 1, chunkY + startY).x
      val y0 = getGroundNode(chunkX + startX, chunkY + startY).y
      val y1 = getGroundNode(chunkX + startX, chunkY + startY + 1).y

      val probeA = ((chunkY + 0) * (numX + 1) + (chunkX + 0)) * LightProbe.SizeInVec4
      val probeB = ((chunkY + 0) * (numX + 1) + (chunkX + 1)) * LightProbe.SizeInVec4
      val probeC = ((chunkY + 1) * (numX + 1) + (chunkX + 0)) * LightProbe.SizeInVec4
      val probeD = ((chunkY + 1) * (numX + 1) + (chunkX + 1)) * LightProbe.SizeInVec4

      val probeI = probeA | probeB << 8 | probeC << 16 | probeD << 24

      var vertY = 0
      var dy = 0.0
      while (vertY <= res) {
        val b = groundVerts
        b.position((baseVert + vertY * (res + 1)) * GroundSpec.sizeInBytes)

        val yf = if (vertY == res)
            y1.toFloat
          else
            (y0 * (1.0 - dy) + y1 * dy).toFloat

        var vertX = 0
        var dx = 0.0
        while (vertX <= res) {

          if (vertX == res)
            b.putFloat(x1.toFloat)
          else
            b.putFloat((x0 * (1.0 - dx) + x1 * dx).toFloat)
          b.putFloat(0.0f)
          b.putFloat(yf)
          b.putFloat(0.0f)
          b.putFloat(1.0f)
          b.putFloat(0.0f)

          b.putInt(probeI)

          val nx = dx
          val ny = dy
          val px = 1.0 - nx
          val py = 1.0 - ny

          b.put((px * py * 255.0).toByte)
          b.put((nx * py * 255.0).toByte)
          b.put((px * ny * 255.0).toByte)
          b.put((nx * ny * 255.0).toByte)

          vertX += 1
          dx += invRes
        }
        vertY += 1
        dy += invRes
      }
    }

    groundVerts.position(0)
    patch.vertices = VertexBuffer.createStatic(GroundSpec, groundVerts)

    MemoryUtil.memFree(groundVerts)

    patch
  }

  def createGroundIndexBuffer(res: Int, numX: Int, numY: Int): IndexBuffer = {
    val numQuads = numX * numY * res * res
    val groundIndices = MemoryUtil.memAlloc(numQuads * 6 * 2)
    val vertsPerChunk = (res + 1) * (res + 1)

    for {
      chunkY <- 0 until numY
      chunkX <- 0 until numX
    } {
      val baseVert = (chunkY * numX + chunkX) * vertsPerChunk

      var vertY = 0
      while (vertY < res) {
        var base = baseVert + vertY * (res + 1)

        var vertX = 0
        while (vertX < res) {

          val a = base.toShort
          val b = (base + 1).toShort
          val c = (base + (res + 1)).toShort
          val d = (base + 1 + (res + 1)).toShort

          groundIndices.putShort(c)
          groundIndices.putShort(b)
          groundIndices.putShort(a)
          groundIndices.putShort(b)
          groundIndices.putShort(c)
          groundIndices.putShort(d)

          base += 1
          vertX += 1
        }
        vertY += 1
      }

    }

    groundIndices.position(0)
    val indexBuffer = IndexBuffer.createStatic(groundIndices)

    MemoryUtil.memFree(groundIndices)

    indexBuffer
  }

  val groundIndices = createGroundIndexBuffer(16, 3, 3)

  val groundPatches = for {
    x <- -10 to 10 by 3
    y <- -10 to 10 by 3
  } yield createGroundPatch(x, y, 16, 3, 3)

  TestModelShader.load()
  TestShadowShader.load()

  val shadowTarget = RenderTarget.create(2048, 2048, None, Some(TexFormat.D24S8), true)
    .withLabel("Shadow Target")

  var renderTarget: RenderTarget = null
  var angle = 0.0

  var toggle = true
  var zoom = 0.5

  val cableMesh: CableMesh = {
    val cables = getCablePaths(asset)
    val cable = cables.head.toBuffer
    cable += new CableNode(Vector3(4.0, 0.2, 5.0), Vector3(5.0, 0.0, 0.0))
    cable += new CableNode(Vector3(8.0, 0.2, 3.0), Vector3(2.0, 0.0, -3.0))
    cable += new CableNode(Vector3(12.0, 0.2, 1.0), Vector3(5.0, 0.0, 0.0))
    cable += new CableNode(Vector3(40.0, 0.2, 5.0), Vector3(5.0, 0.0, 0.0))
    CableRenderSystem.createCableMesh(cable, 0.2)
  }

  val startTime = AppWindow.currentTime
  var prevTime = startTime
  while (AppWindow.running) {
    val renderer = Renderer.get

    AppWindow.pollEvents()

    val time = AppWindow.currentTime - startTime
    val dt = time - prevTime
    prevTime = time

    val viewWidth = AppWindow.width
    val viewHeight = AppWindow.height

    if (viewWidth != prevWidth || viewHeight != prevHeight) {
      renderer.resizeBackbuffer(viewWidth, viewHeight)

      if (renderTarget != null) renderTarget.unload()
      renderTarget = RenderTarget.create(viewWidth, viewHeight, Some("SRGB"), Some("D24S"), false, 8)
      renderTarget.setLabel("Multisample target")
    }

    if (mapping.isDown(DebugInput.Down)) zoom += dt * 2.0
    if (mapping.isDown(DebugInput.Up)) zoom -= dt * 2.0
    zoom = clamp(zoom, 0.5, 1.5)

    val viewPos = Vector3(math.sin(angle) * 18.0, 30.0, math.cos(angle) * -18.0) * zoom
    val viewProjection = (
      Matrix4.perspective(viewWidth.toDouble / viewHeight.toDouble, math.Pi / 3.0, 0.01, 1000.0)
        * Matrix43.look(viewPos, Vector3(-math.sin(angle), -1.5, math.cos(angle))))

    val shadowViewProjection = (
      Matrix4.orthographic(80.0, 80.0, 0.1, 100.0)
        * Matrix43.look(Vector3(0.25, 0.75, -0.25) * 40.0, -Vector3(0.25, 0.75, -0.25)))

    head.localTransform = Matrix43.rotateZ(math.sin(time * 0.6) * 0.5) * Matrix43.rotateX(math.sin(time * 0.5) * 0.2)
    barrel.localTransform = Matrix43.rotateY(time * -15.0)

    LightSystem.addDynamicLightsToCells()
    LightSystem.evaluateProbes()
    LightSystem.finishFrame()

    ModelSystem.updateMatrices()
    ModelSystem.collectMeshInstances()
    ModelSystem.setupUniforms()

    val draws = ModelSystem.getInstancedMesheDraws()

    renderer.advanceFrame()

    renderer.setRenderTarget(shadowTarget)
    renderer.setDepthMode(true, true)
    renderer.setWriteSrgb(false)
    renderer.clear(None, Some(1.0))

    renderer.pushUniform(TestShadowShader.ShadowUniform, u => {
      TestShadowShader.ShadowUniform.ViewProjection.set(u, shadowViewProjection)
    })

    TestShadowShader.get.use()

    for (draw <- draws) {
      val mesh = draw.mesh
      val part = mesh.parts.head
      assert(draw.mesh.parts.length == 1)

      renderer.bindUniform(ModelSystem.InstancedUniform, draw.instanceUbo)

      val numElems = part.numIndices
      renderer.drawElementsInstanced(draw.num, numElems, part.indexBuffer, part.vertexBuffer)
    }

    renderer.setRenderTarget(renderTarget)
    renderer.setDepthMode(true, true)
    renderer.setWriteSrgb(true)
    renderer.clear(Some(Color.rgb(0x6495ED)), Some(1.0))

    renderer.pushUniform(GlobalUniform, u => {
      GlobalUniform.ViewProjection.set(u, viewProjection)
      GlobalUniform.ShadowViewProjection.set(u, shadowViewProjection)
      GlobalUniform.ViewPosition.set(u, viewPos, 0.0f)
    })

    renderer.setCull(true)

    val shader = TestModelShader.get
    shader.use(p => {
      if (toggle) {
        p(TestModelShader.Permutations.BrdfFunc_D) = 2
        p(TestModelShader.Permutations.BrdfFunc_V) = 2
        p(TestModelShader.Permutations.BrdfFunc_F) = 2
      } else {
        p(TestModelShader.Permutations.BrdfFunc_D) = 2
        p(TestModelShader.Permutations.BrdfFunc_V) = 1
        p(TestModelShader.Permutations.BrdfFunc_F) = 1
      }
    })

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

      renderer.bindUniform(ModelSystem.InstancedUniform, draw.instanceUbo)
      renderer.bindUniform(ModelSystem.LightProbeUniform, draw.lightProbeUbo)

      val numElems = part.numIndices
      renderer.drawElementsInstanced(draw.num, numElems, part.indexBuffer, part.vertexBuffer)
    }

    TestCableShader.get.use()

    renderer.pushUniform(TestCableShader.CableUniform, u => {
      TestCableShader.CableUniform.Pulse.set(u, time.toFloat, 0.0f, 0.0f, 0.0f)
    })

    renderer.setTextureTargetDepth(TestCableShader.Textures.ShadowMap, shadowTarget)
    cableMesh.draw()

    renderer.setTexture(ModelTextures.Albedo, ground_albedo.get.texture)
    renderer.setTexture(ModelTextures.Normal, ground_normal.get.texture)
    renderer.setTexture(ModelTextures.Roughness, ground_roughness.get.texture)
    renderer.setTexture(ModelTextures.Metallic, ground_metallic.get.texture)
    renderer.setTexture(ModelTextures.AoTex, ground_ao.get.texture)
    TestGroundShader.get.use(p => {
      if (toggle) {
        p(TestGroundShader.Permutations.BrdfFunc_D) = 2
        p(TestGroundShader.Permutations.BrdfFunc_V) = 2
        p(TestGroundShader.Permutations.BrdfFunc_F) = 2
      } else {
        p(TestGroundShader.Permutations.BrdfFunc_D) = 2
        p(TestGroundShader.Permutations.BrdfFunc_V) = 1
        p(TestGroundShader.Permutations.BrdfFunc_F) = 1
      }
    })

    for (patch <- groundPatches) {
      renderer.pushUniform(ModelSystem.LightProbeUniform, u => {
        val stride = ModelSystem.LightProbeUniform.LightProbes.arrayStrideInBytes
        var base = ModelSystem.LightProbeUniform.LightProbes.offsetInBytes
        val baseStride = LightProbe.SizeInVec4 * stride
        for (probe <- patch.probes) {
          probe.writeToUniform(u, base, stride)
          base += baseStride
        }
      })
      renderer.drawElements(groundIndices.numIndices, groundIndices, patch.vertices)
    }

    if (mapping.justPressed(DebugInput.Reload)) {
      processResources()
      AssetLoader.reloadEverything()
      ModelSystem.assetsLoaded()
    }

    if (mapping.isDown(DebugInput.Left)) {
      angle += dt * 2.0
    }
    if (mapping.isDown(DebugInput.Right)) {
      angle -= dt * 2.0
    }

    val cables = getCablePaths(asset)

    if (mapping.justPressed(DebugInput.Toggle))
      toggle = !toggle

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

    renderer.setBlend(true)
    renderer.setDepthMode(false, false)

    {
      val draws = ArrayBuffer[ui.Font.TextDraw]()

      {
        val text = s"Toggle: $toggle"
        draws += ui.Font.TextDraw(text, 0, text.length, Vector2(100.0, 134.0), 22.0, Color.rgb(0xFFFFFF), 0.0, 1)
      }

      fontAsset.get.render(draws)
    }

    renderer.blitRenderTargetColor(RenderTarget.Backbuffer, renderTarget)

    renderer.setRenderTarget(RenderTarget.Backbuffer)


    AppWindow.swapBuffers()
  }

  AppWindow.unload()
}

