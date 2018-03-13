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
import CableRenderSystem.CableNode

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

  val asset = ModelAsset("game/tower/tower_radar.fbx.s2md")

  val entity = new Entity()
  val model = ModelSystem.addModel(entity, asset)
  entity.position = Vector3(0.0, 0.0, 0.0)

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


  object DebugInput extends InputSet("Debug") {
    val Reload = button("Reload")
    val Left = button("Left")
    val Right = button("Right")
  }

  val fontAsset = FontAsset("font/open-sans/OpenSans-Regular.ttf.s2ft")

  val keyboard = AppWindow.keyboard
  val debugMapping = Toml.parse(
    """
      |[Keyboard.Debug]
      |Reload = "R"
      |Left = "A"
      |Right = "D"
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

    uniform(PixelUniform)
    object PixelUniform extends UniformBlock("PixelUniform") {
      val UvBounds = vec4("UvBounds")
    }

    override val Textures = ModelTextures
  }

  var renderTarget: RenderTarget = null
  var angle = 0.0

  var toggle = false

  val startTime = AppWindow.currentTime
  var prevTime = startTime
  while (AppWindow.running) {
    val renderer = Renderer.get

    AppWindow.pollEvents()

    val time = AppWindow.currentTime - startTime
    val dt = time - prevTime
    prevTime = time

    radar.localTransform = Matrix43.rotateZ(time)

    val viewWidth = AppWindow.width
    val viewHeight = AppWindow.height

    if (viewWidth != prevWidth || viewHeight != prevHeight) {
      renderer.resizeBackbuffer(viewWidth, viewHeight)

      if (renderTarget != null) renderTarget.unload()
      renderTarget = RenderTarget.create(viewWidth, viewHeight, Some("SRGB"), Some("D24S"), false, 8)
      renderTarget.setLabel("Multisample target")
    }


    val viewProjection = (
      Matrix4.perspective(viewWidth.toDouble / viewHeight.toDouble, math.Pi / 3.0, 0.01, 1000.0)
        * Matrix43.look(Vector3(math.sin(angle) * 12.0, 10.0, math.cos(angle) * -12.0), Vector3(-math.sin(angle), -0.5, math.cos(angle))))

    renderer.advanceFrame()
    renderer.setRenderTarget(renderTarget)
    renderer.setDepthMode(true, true)
    renderer.setWriteSrgb(true)
    renderer.clear(Some(Color.rgb(0x6495ED)), Some(1.0))

    ModelSystem.updateMatrices()
    ModelSystem.collectMeshInstances()
    ModelSystem.setupUniforms()
    val draws = ModelSystem.getInstancedMesheDraws()

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

      renderer.pushUniform(TestModelShader.PixelUniform, u => {
        TestModelShader.PixelUniform.UvBounds.set(u, part.uvOffsetX, part.uvOffsetY, part.uvScaleX, part.uvScaleY)
      })

      renderer.setTexture(ModelTextures.Diffuse, mesh.material.albedoTex.texture)
      renderer.bindUniform(ModelSystem.InstancedUniform, draw.instanceUbo)

      val numElems = part.numIndices
      renderer.drawElementsInstanced(draw.num, numElems, part.indexBuffer, part.vertexBuffer)
    }

    if (mapping.justPressed(DebugInput.Reload) && false) {
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

    if (mapping.justPressed(DebugInput.Reload))
      toggle = !toggle

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
      val text = s"Points: $totalPoints"
      draws += ui.Font.TextDraw(text, 0, text.length, Vector2(100.0, 114.0), 22.0, Color.rgb(0xFFFFFF), 0.0, 1)
      fontAsset.get.render(draws)
    }

    renderer.blitRenderTargetColor(RenderTarget.Backbuffer, renderTarget)

    renderer.setRenderTarget(RenderTarget.Backbuffer)


    AppWindow.swapBuffers()
  }

  AppWindow.unload()
}

