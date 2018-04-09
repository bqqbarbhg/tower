package game.state

import core._
import util.BufferUtils._
import render._
import asset._
import game.shader._
import platform.AppWindow
import MenuState._
import gfx.Material
import menu.{DebugMenu, OptionsMenu}
import ui._
import ui.Canvas._
import ui.InputSet.InputArea
import io.property._
import org.lwjgl.system.MemoryUtil
import game.system._
import game.system.audio.AudioSystem.SoundRef
import game.system.rendering.ModelSystem.ModelInstance
import game.system.rendering._
import game.system.audio._
import locale.LocaleString._
import main.GameStartup
import util.geometry.Frustum

import scala.collection.mutable.ArrayBuffer

object MenuState {

  val MenuAtlas = AtlasAsset("atlas/menu.s2at")
  val StatueModel = ModelAsset("mainmenu/mainmenu_statue.fbx.s2md")
  val MainFont = FontAsset("font/catamaran/Catamaran-SemiBold.ttf.s2ft")
  val MainColorgrade = TextureAsset("colorgrade/mainmenu.png.s2tx")

  val MenuMusic = SoundAsset("audio/music/mainmenu.ogg.s2au")

  private val tMenuItem = TextStyle(MainFont, 44.0)

  private val lMain = 0

  val Assets = Vector(
    StatueModel,
    MainFont,
    SimpleMeshShader,
    TonemapShader,
    MenuAtlas,
    Material.shared,
    MainColorgrade,
    MenuMusic,
  )

  val menuAssets = new AssetBundle("MenuState", Assets ++ OptionsMenu.Assets)

  class Button(val localeKey: String) {
    val text = lc"menu.mainmenu.button.$localeKey"
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

  var turretModel: ModelInstance = _
  var startTime = 0.0

  val canvas = new Canvas()
  val inputSet = new InputSet()

  val ContinueButton = new Button("continue")
  val NewGameButton = new Button("newGame")
  val OptionsButton = new Button("options")
  val ExitButton = new Button("exit")

  val buttons = Array(ContinueButton, NewGameButton, OptionsButton, ExitButton)

  var optionsMenu: Option[OptionsMenu] = None

  var music: SoundRef = null
  var modelEntity: Entity = null

  override def load(): Unit = {
    menuAssets.acquire()
    GameState.push(new LoadingState())
  }

  override def start(): Unit = {
    base.loadState()
    rendering.loadState()

    modelEntity = new Entity(true, "Mainmenu Model")
    turretModel = modelSystem.addModel(modelEntity, StatueModel)

    startTime = AppWindow.currentTime
    music = audioSystem.play(MenuMusic, AudioSystem.Music)
    music.instance.setFullLoop()
  }

  override def stop(): Unit = {

    base.entitySystem.deleteAllEntities()

    rendering.unloadState()
    base.unloadState()

    menuAssets.release()
    music.stop()
}

  override def update(): Unit = {
    AppWindow.pollEvents()

    val time = AppWindow.currentTime

    val renderer = Renderer.get
    renderer.beginFrame()
    renderer.setRenderTarget(globalRenderSystem.mainTargetMsaa)
    renderer.setDepthMode(true, true)
    renderer.clear(Some(Color.rgb(0x707070)), Some(1.0))
    renderer.setBlend(Renderer.BlendNone)


    inputSet.update()

    if (OptionsButton.input.clicked) {
      optionsMenu = Some(new OptionsMenu(inputSet, canvas))
    }

    if (ExitButton.input.clicked) {
      GameStartup.exitRequested = true
    }

    for (menu <- optionsMenu) {
      if (menu.wantsToClose)
        optionsMenu = None
    }

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

    for (menu <- optionsMenu)
      menu.update()
    // debugMenu.update(div.copy.padAround(100.0).pushLeft(200.0))

    if (optionsMenu.isEmpty) {
      div.pushLeft(1280.0 * 0.1)
      val options = div.pushLeft(200.0)
      options.pushTop(300.0)

      for (button <- buttons) {
        val pos = options.pushTop(40.0)

        inputSet.add(0, button.input, pos)

        var style = tMenuItem.copy(height = pos.heightPx, color = Color.White.copy(a = 0.5))
        if (button.input.focused) {
          style = style.copy(color = style.color.copy(a = 0.75))
        }
        canvas.drawText(lMain, style, pos.x0, pos.y0, button.text)
        options.padTop(20.0)
      }

    }

    renderer.pushUniform(SimpleMeshShader.GlobalUniform, u => {
      SimpleMeshShader.GlobalUniform.ViewProjection.set(u, viewProj)
    })

    val visibleSet = new EntitySet()
    visibleSet.add(modelEntity)
    val models = modelSystem.collectVisibleModels(Some(modelEntity))
    modelSystem.updateModels(models)
    val meshes = modelSystem.collectMeshInstances(models)

    for {
      (part, instances) <- meshes.meshes
      instance <- instances
    } {
      renderer.setTexture(SimpleMeshShader.Textures.Texture, part.mesh.material.albedoTex.texture)
      renderer.pushUniform(SimpleMeshShader.InstanceUniform, u => {
        SimpleMeshShader.InstanceUniform.World.set(u, instance.worldTransform)
        SimpleMeshShader.InstanceUniform.UvBounds.set(u, part.uvOffsetX, part.uvOffsetY, part.uvScaleX, part.uvScaleY)
      })

      part.draw()
    }


    renderer.setDepthMode(false, false)
    renderer.setCull(false)
    renderer.setBlend(Renderer.BlendNone)
    renderer.setRenderTarget(globalRenderSystem.msaaResolveTarget)

    TonemapShader.get.use()

    if (globalRenderSystem.msaa > 1)
      renderer.setTextureTargetColor(TonemapShader.Textures.BackbufferMsaa, globalRenderSystem.mainTargetMsaa, 0)
    else
      renderer.setTextureTargetColor(TonemapShader.Textures.Backbuffer, globalRenderSystem.mainTargetMsaa, 0)

    renderer.setTexture(TonemapShader.Textures.ColorLookup, MainColorgrade.get.texture)

    renderer.drawQuad()

    renderer.setRenderTarget(RenderTarget.Backbuffer)
    renderer.clear(Some(Color.Black), None)

    PostprocessShader.get.use()

    renderer.setTextureTargetColor(TonemapShader.Textures.Backbuffer, globalRenderSystem.msaaResolveTarget, 0)

    renderer.drawQuad()

    canvas.render()

    LayoutDebugger.render()

    renderer.endFrame()

    AppWindow.swapBuffers()
  }

  override def done: Boolean = false

}

