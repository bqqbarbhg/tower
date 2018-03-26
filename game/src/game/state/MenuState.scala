package game.state

import core._
import render._
import asset._
import game.shader._
import platform.AppWindow
import MenuState._
import game.lighting.Tonemapper
import game.system.{ModelSystem, RenderingSystem}
import game.system.ModelSystem.ModelRef
import gfx.Material
import menu.{DebugMenu, OptionsMenu}
import ui._
import ui.Canvas._
import ui.InputSet.InputArea
import io.property._

object MenuState {

  val MenuAtlas = AtlasAsset("atlas/menu.s2at")
  val StatueModel = ModelAsset("mainmenu/mainmenu_statue.fbx.s2md")
  val MainFont = FontAsset("font/open-sans/OpenSans-Regular.ttf.s2ft")

  private val tMenuItem = TextStyle(MainFont, 44.0)

  private val lMain = 0

  val menuAssets = new AssetBundle(
    "MenuState",
    StatueModel,
    MainFont,
    SimpleMeshShader,
    MenuAtlas,
    Material.shared,
  )

  class Button(val localeKey: String) {
    val input = new InputArea()
  }

  object Tweak extends PropertyContainer {
    private val arr = MacroPropertySet.make[Tweak.type]()
    override val propertySet: PropertySet = new PropertySet("MenuState.Tweak", arr) {
      range("cameraFov", 30.0, 90.0)
    }

    var cameraFov: DoubleProp.Type = 45.0
    var testText: StringProp.Type = "Hello world!"
  }

}

class MenuState extends GameState {

  var turretModel: ModelRef = _
  var startTime = 0.0

  val canvas = new Canvas()
  val inputSet = new InputSet()

  val ContinueButton = new Button("MainMenu.continue")
  val ExitButton = new Button("MainMenu.exit")

  val buttons = Array(ContinueButton, ExitButton)

  val optionsMenu = new OptionsMenu(inputSet, canvas)
  val debugMenu = new DebugMenu(inputSet, canvas, Tweak)

  override def load(): Unit = {
    menuAssets.acquire()
    GameState.push(new LoadingState())
  }

  override def start(): Unit = {
    turretModel = ModelSystem.addModel(null, StatueModel)
    turretModel.useManualDraws = true
    startTime = AppWindow.currentTime
  }

  override def stop(): Unit = {
    menuAssets.release()
}

  override def update(): Unit = {
    AppWindow.pollEvents()

    val time = AppWindow.currentTime

    val renderer = Renderer.get
    renderer.beginFrame()
    renderer.setRenderTarget(RenderingSystem.MainTargetMsaa)
    renderer.setDepthMode(true, true)
    renderer.clear(Some(Color.rgb(0x707070)), Some(1.0))
    renderer.setBlend(Renderer.BlendNone)

    ModelSystem.updateMatrices()
    ModelSystem.collectMeshInstances()
    ModelSystem.setupUniforms()

    inputSet.update()

    val shader = SimpleMeshShader.get
    shader.use()

    val relTime = time - startTime
    val tt = relTime * 6.0
    val ddx = math.sin(tt * 0.1) * 2.0 + math.sin(tt * 0.13 + 7.0) + math.sin(tt * 0.07 + 2.0) * 0.5
    val ddy = math.sin(tt * 0.12) * 2.0 + math.sin(tt * 0.06 + 4.0) + math.sin(tt * 0.04 + 2.0) * 0.5
    val dx = ddx * 0.1
    val dy = ddy * 0.1

    val angle = relTime * 0.02 + 0.2
    val xx = math.sin(angle)
    val yy = math.cos(angle)

    val offset = Vector3(yy * 4.0, 0.0, xx * 4.0)
    val pos = Vector3(xx * 12.0, 7.5, yy * -12.0)
    val target = Vector3(dx, 3.0, dy)
    val view = Matrix43.look(pos + offset, target - pos)
    val fov = math.toRadians(Tweak.cameraFov)
    val proj = Matrix4.perspective(renderer.currentRenderTarget.aspectRatio, fov, 0.1, 50.0)
    val viewProj = proj * view

    val div = Layout.screen720p

    // optionsMenu.update()
    debugMenu.update(div.copy.padAround(100.0).pushLeft(200.0))

    if (false) {
      div.pushLeft(1280.0 * 0.1)
      val options = div.pushLeft(200.0)
      options.pushTop(300.0)

      for (button <- buttons) {
        val pos = options.pushTop(40.0)

        inputSet.add(0, button.input, pos)

        var style = tMenuItem.copy(height = pos.heightPx, color = Color.White.copy(a = 0.5))
        if (button.input.focused) {
          style = style.copy(color = Color.rgb(0xFF0000))
        }
        canvas.drawText(lMain, style, pos.x0, pos.y0, button.localeKey)
        options.padTop(20.0)
      }

    }

    renderer.pushUniform(SimpleMeshShader.GlobalUniform, u => {
      SimpleMeshShader.GlobalUniform.ViewProjection.set(u, viewProj)
    })

    for (draw <- turretModel.manualDraws) {
      for (part <- draw.mesh.parts) {
        renderer.setTexture(SimpleMeshShader.Textures.Texture, draw.mesh.material.albedoTex.texture)
        renderer.pushUniform(SimpleMeshShader.InstanceUniform, u => {
          SimpleMeshShader.InstanceUniform.World.set(u, draw.worldTransform)
          SimpleMeshShader.InstanceUniform.UvBounds.set(u, part.uvOffsetX, part.uvOffsetY, part.uvScaleX, part.uvScaleY)
        })

        part.draw()
      }
    }

    renderer.blitRenderTargetColor(RenderTarget.Backbuffer, RenderingSystem.MainTargetMsaa)
    renderer.setRenderTarget(RenderTarget.Backbuffer)

    renderer.setDepthMode(false, false)

    canvas.render()

    LayoutDebugger.render()

    renderer.endFrame()

    AppWindow.swapBuffers()
  }

  override def done: Boolean = false

}

