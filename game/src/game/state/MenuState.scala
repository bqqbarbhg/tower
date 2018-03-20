package game.state

import core._
import render._
import asset._
import game.shader._
import platform.AppWindow
import MenuState._
import game.system.{ModelSystem, RenderingSystem}
import game.system.ModelSystem.ModelRef

object MenuState {

  val TurretModel = ModelAsset("mainmenu/tower_turret_mainmenu.fbx.s2md")
  val TurretTexture = TextureAsset("mainmenu/tower_turret_mainmenu_ao.png.s2tx")

  val menuAssets = new AssetBundle(
    "MenuState",
    TurretModel, TurretTexture, SimpleMeshShader
  )

}

class MenuState extends GameState {

  var turretModel: ModelRef = _

  override def load(): Unit = {
    menuAssets.acquire()
    GameState.push(new LoadingState())
  }

  override def start(): Unit = {
    turretModel = ModelSystem.addModel(null, TurretModel)
    turretModel.useManualDraws = true
  }

  override def stop(): Unit = {
    menuAssets.release()
  }

  override def update(): Unit = {
    AppWindow.pollEvents()

    val renderer = Renderer.get
    renderer.beginFrame()
    renderer.setRenderTarget(RenderingSystem.MainTargetMsaa)
    renderer.clear(Some(Color.rgb(0x6495ED)), Some(1.0))
    renderer.setDepthMode(true, true)
    renderer.setBlend(Renderer.BlendNone)

    ModelSystem.updateMatrices()
    ModelSystem.collectMeshInstances()
    ModelSystem.setupUniforms()

    val shader = SimpleMeshShader.get
    shader.use()

    val pos = Vector3(0.0, 5.0, -15.0)
    val view = Matrix43.look(pos, -pos)
    val proj = Matrix4.perspective(renderer.currentRenderTarget.aspectRatio, math.Pi / 3.0, 0.1, 50.0)
    val viewProj = proj * view

    renderer.setTexture(SimpleMeshShader.Textures.Texture, TurretTexture.get.texture)
    renderer.pushUniform(SimpleMeshShader.GlobalUniform, u => {
      SimpleMeshShader.GlobalUniform.ViewProjection.set(u, viewProj)
    })

    for (draw <- turretModel.manualDraws) {
      for (part <- draw.mesh.parts) {
        renderer.pushUniform(SimpleMeshShader.InstanceUniform, u => {
          SimpleMeshShader.InstanceUniform.World.set(u, draw.worldTransform)
          SimpleMeshShader.InstanceUniform.UvBounds.set(u, part.uvOffsetX, part.uvOffsetY, part.uvScaleX, part.uvScaleY)
        })

        part.draw()
      }
    }

    renderer.blitRenderTargetColor(RenderTarget.Backbuffer, RenderingSystem.MainTargetMsaa)

    renderer.endFrame()

    AppWindow.swapBuffers()
  }

  override def done: Boolean = false

}

