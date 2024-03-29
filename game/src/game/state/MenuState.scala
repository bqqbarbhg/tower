package game.state

import core._
import util.BufferUtils._
import render._
import asset._
import game.shader._
import platform.{AppWindow, KeyEvent}
import MenuState._
import game.component.CampaignComponent
import gfx.Material
import menu.{DebugMenu, OptionsMenu}
import ui._
import ui.Canvas._
import ui.InputSet.InputArea
import io.property._
import game.system._
import game.system.audio.AudioSystem.SoundRef
import game.system.rendering.ModelSystem.ModelInstance
import game.system.rendering._
import game.system.audio._
import locale.Locale
import locale.LocaleString._
import main.GameStartup
import util.geometry.Frustum
import render.Renderer._
import task.Task

import scala.collection.mutable.ArrayBuffer

object MenuState {

  val MenuAtlas = AtlasAsset("atlas/menu.s2at")
  val StatueModel = ModelAsset("mainmenu/mainmenu_statue.fbx.s2md")
  val MainFont = FontAsset("font/catamaran/Catamaran-SemiBold.ttf.s2ft")
  val CreditsFont = FontAsset("font/open-sans/OpenSans-Regular.ttf.s2ft")
  val MainColorgrade = TextureAsset("colorgrade/mainmenu.png.s2tx")
  val CreditsColorgrade = TextureAsset("colorgrade/credits.png.s2tx")

  val MenuMusic = SoundAsset("audio/music/mainmenu.ogg.s2au")

  private val tMenuItem = TextStyle(MainFont, 44.0)
  private val tCampaignItem = TextStyle(MainFont, 38.0)
  private val tCampaignTitle = TextStyle(MainFont, 24.0, color = Color.rgba(0xFFFFFF, 0.5))
  private val tCredits = TextStyle(CreditsFont, 22.0)

  private val lMain = 0

  val Assets = new AssetBundle("MenuState",
    StatueModel,
    MainFont,
    SimpleMeshShader,
    TonemapShader,
    PostprocessShader,
    MenuAtlas,
    Material.shared,
    MainColorgrade,
    CreditsColorgrade,
    MenuMusic,
    OptionsMenu.Assets,
  )

  class Button(val localeKey: String) {
    val text = lc"menu.mainmenu.button.$localeKey"
    val input = new InputArea()
    var visible: Boolean = true
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
  val CreditsButton = new Button("credits")
  val ExitButton = new Button("exit")

  val CreditsBackButton = new Button("creditsBack")

  val buttons = Array(ContinueButton, NewGameButton, OptionsButton, CreditsButton, ExitButton)

  var optionsMenu: Option[OptionsMenu] = None

  var music: SoundRef = null
  var modelEntity: Entity = null

  var finished: Boolean = false

  var creditsOpen: Boolean = false
  var creditsScroll: Double = 0.0
  var creditsWindup: Double = 0.0

  var prevTime: Double = 0.0

  var campaignLoadTask: Task[Array[EntityType]] = null
  var campaigns: Array[EntityType] = Array[EntityType]()

  val campaignSelectInput = new InputArea()
  val campaignCancelInput = new InputArea()

  var showCampaignSelect: Boolean = false

  var preloadedSave: Option[PlayState.PreloadInfo] = None

  override def load(): Unit = {

    campaignLoadTask = Task.Main.add(() => {
      val pack = io.content.Package.get
      pack.list("entity/campaign")
        .filter(_.name.endsWith(".s2es"))
        .map(file => {
          val asset = EntityTypeAsset(file.name)
          asset.getShallowUnsafe
        })
        .toArray
        .sortBy(et => {
          et.find(CampaignComponent).map(_.sortIndex).getOrElse(0)
        })
    })

    Assets.acquire()
    GameState.push(new LoadingState())
  }

  override def start(): Unit = {
    campaigns = campaignLoadTask.get
    preloadedSave = PlayState.preloadSave(campaigns)

    ContinueButton.visible = preloadedSave.isDefined

    base.loadState()
    rendering.loadState()

    modelEntity = new Entity(true, "Mainmenu Model")
    turretModel = modelSystem.addModel(modelEntity, StatueModel)

    startTime = AppWindow.currentTime
    music = audioSystem.play(MenuMusic, AudioSystem.Music)
    music.instance.setFullLoop()

    prevTime = AppWindow.currentTime
  }

  override def stop(): Unit = {

    music.stop()
    audioSystem.update()

    base.entitySystem.deleteAllEntities()

    rendering.unloadState()
    base.unloadState()

    Assets.release()
}

  override def update(): Unit = {
    AppWindow.pollEvents()

    val time = AppWindow.currentTime
    val dt = time - prevTime
    prevTime = time

    val renderer = Renderer.get
    renderer.beginFrame()
    renderer.setWriteSrgb(false)
    renderer.setRenderTarget(globalRenderSystem.mainTargetMsaa)

    renderer.setMode(DepthWrite, BlendNone, CullNormal)

    renderer.clear(Some(Color.rgb(0x707070)), Some(1.0))

    inputSet.update()

    if (ContinueButton.input.clicked) {
      finished = true
      GameState.push(new PlayState(PlayState.StartLastSave(preloadedSave.get)))
    }

    if (NewGameButton.input.clicked) {
      showCampaignSelect = true
    }

    if (OptionsButton.input.clicked) {
      optionsMenu = Some(new OptionsMenu(inputSet, canvas))
    }

    if (CreditsButton.input.clicked) {
      creditsOpen = true
      creditsScroll = 0.0
      creditsWindup = -1.0
    }

    if (ExitButton.input.clicked) {
      GameStartup.exitRequested = true
    }

    if (CreditsBackButton.input.clicked) {
      creditsOpen = false
    }

    if (campaignCancelInput.clicked) {
      showCampaignSelect = false
    }

    if (campaignSelectInput.clickIndex >= 0) {
      val index = campaignSelectInput.clickIndex
      val campaign = campaigns(index).asset.get
      val start = PlayState.StartCampaign(campaign)
      finished = true
      GameState.push(new PlayState(start))
    }

    for (menu <- optionsMenu) {
      if (menu.wantsToClose)
        optionsMenu = None
    }

    if (AppWindow.keyDownEvents.exists(_.key == KeyEvent.Escape)) {
      creditsOpen = false
      showCampaignSelect = false
      for (opt <- optionsMenu)
        opt.wantsToClose = true
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

    val offset = if (creditsOpen)
      Vector3(-yy * 7.0, 0.0, -xx * 4.0)
    else
      Vector3(yy * 4.0, 0.0, xx * 4.0)

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

    if (optionsMenu.isEmpty && !creditsOpen && !showCampaignSelect) {
      div.pushLeft(1280.0 * 0.1)
      val options = div.pushLeft(200.0)
      options.pushTop(300.0)

      for (button <- buttons) {
        val pos = options.pushTop(40.0)

        if (button.visible) {
          inputSet.add(0, button.input, pos)

          var style = tMenuItem.copy(height = pos.heightPx, color = Color.White.copy(a = 0.5))
          if (button.input.focused) {
            style = style.copy(color = style.color.copy(a = 0.75))
          }
          canvas.drawText(lMain, style, pos.x0, pos.y0, button.text)
        }

        options.padTop(20.0)
      }
    }

    if (showCampaignSelect) {
      div.pushLeft(1280.0 * 0.1)
      val options = div.pushLeft(300.0)
      options.padTop(200.0)

      {
        val pos = options.pushTop(40.0)
        canvas.drawText(lMain, tCampaignTitle, pos, lc"menu.mainmenu.selectCampaign")
      }

      for ((campaign, index) <- campaigns.zipWithIndex) {
        val campC = campaign.find(CampaignComponent).get

        val pos = options.pushTop(40.0)

        inputSet.add(0, campaignSelectInput, pos, 0.0, index)

        var style = tCampaignItem.copy(color = Color.White.copy(a = 0.5))
        if (campaignSelectInput.focusIndex == index) {
          style = style.copy(color = style.color.copy(a = 0.75))
        }

        val name = if (campC.name.nonEmpty) campC.name else {
          Locale.getSimple(campC.locale)
        }
        canvas.drawText(lMain, style, pos, name)
      }

      options.padBottom(150.0)

      {
        val pos = options.pushBottom(40.0)
        inputSet.add(0, campaignCancelInput, pos)

        var style = tCampaignItem.copy(color = Color.White.copy(a = 0.5))
        if (campaignCancelInput.focused) {
          style = style.copy(color = style.color.copy(a = 0.75))
        }

        canvas.drawText(lMain, style, pos, lc"menu.mainmenu.button.cancelGame")
      }
    }

    if (creditsOpen) {

      val scroll = AppWindow.mouseScroll

      if (math.abs(scroll) > 0.01) {
        creditsWindup = -1.0
      }

      creditsScroll += scroll * -60.0
      creditsScroll += dt * 20.0 * math.max(creditsWindup, 0.0)
      creditsWindup = math.min(creditsWindup + dt * 0.5, 1.0)

      val text = lc"menu.credits.text"
      val approxHeight = text.count(_ == '\n') * 22.0

      creditsScroll = clamp(creditsScroll, 0.0, approxHeight - 700.0)

      val mutLayout = Layout.screen720p.padLeft(50.0)
      val layout = mutLayout.pushRight(600.0).extendTop(creditsScroll).extendBottom(40.0)
      val backButton = mutLayout.padRight(20.0).pushRight(200.0).padTop(100.0)
      val pos = backButton.pushTop(40.0)

      {
        val button = CreditsBackButton
        inputSet.add(0, button.input, pos)

        var style = tMenuItem.copy(height = pos.heightPx, color = Color.White.copy(a = 0.5))
        if (button.input.focused) {
          style = style.copy(color = style.color.copy(a = 0.75))
        }
        canvas.drawText(lMain, style, pos.x0, pos.y0, button.text)
      }

      canvas.drawTextWrapped(0, tCredits, layout, text)
    }

    renderer.pushUniform(SimpleMeshShader.GlobalUniform, u => {
      SimpleMeshShader.GlobalUniform.ViewProjection.set(u, viewProj)
    })

    audioSystem.update()

    val visibleSet = new EntitySet()
    visibleSet.add(modelEntity)
    val models = modelSystem.collectModels(modelEntity)
    modelSystem.updateModels(models)
    val meshes = modelSystem.collectMeshInstances(models)
    modelSystem.frameCleanup()

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

    renderer.setMode(DepthNone, BlendNone, CullNone)
    renderer.setWriteSrgb(false)
    renderer.setRenderTarget(globalRenderSystem.msaaResolveTarget)

    TonemapShader.get.use()

    if (globalRenderSystem.msaa > 1)
      renderer.setTextureTargetColor(TonemapShader.Textures.BackbufferMsaa, globalRenderSystem.mainTargetMsaa, 0)
    else
      renderer.setTextureTargetColor(TonemapShader.Textures.Backbuffer, globalRenderSystem.mainTargetMsaa, 0)

    val colorgrade = if (creditsOpen) CreditsColorgrade else MainColorgrade
    renderer.setTexture(TonemapShader.Textures.ColorLookup, colorgrade.get.texture)

    renderer.drawQuad()

    renderer.setRenderTarget(RenderTarget.Backbuffer)
    renderer.clear(Some(Color.Black), None)

    renderer.setWriteSrgb(false)
    PostprocessShader.get.use()

    renderer.setTextureTargetColor(TonemapShader.Textures.Backbuffer, globalRenderSystem.msaaResolveTarget, 0)

    renderer.drawQuad()

    renderer.setWriteSrgb(true)
    canvas.render()

    LayoutDebugger.render()

    renderer.endFrame()

    AppWindow.swapBuffers()
  }

  override def done: Boolean = finished

}

