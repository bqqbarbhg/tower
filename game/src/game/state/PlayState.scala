package game.state

import core._
import render._
import ui._
import asset._
import platform.{AppWindow, KeyEvent}
import game.system
import game.system.base._
import game.system.rendering._
import menu.PauseMenu
import PlayState._

object PlayState {

  val Assets = new AssetBundle("PlayState", PauseMenu.Assets)
}

class PlayState extends GameState {

  val inputs = new InputSet()
  val canvas = new Canvas()
  val pauseMenu = new PauseMenu(inputs, canvas)
  var prevTime = 0.0
  var finished: Boolean = false

  override def load(): Unit = {
    Assets.acquire()
    GameState.push(new LoadingState())
  }

  override def start(): Unit = {

    system.base.loadState()
    system.rendering.loadState()
    system.rendering.loadGame()

    prevTime = AppWindow.currentTime
  }
  override def stop(): Unit = {
    entitySystem.deleteAllEntities()

    system.rendering.unloadGame()
    system.rendering.unloadState()
    system.base.unloadState()

    Assets.release()
  }

  override def update(): Unit = {
    AppWindow.pollEvents()

    val time = AppWindow.currentTime
    val dt = time - prevTime
    prevTime = time

    for (e <- AppWindow.keyDownEvents) {
      if (e.key == KeyEvent.Escape) {
        pauseMenu.isOpen = !pauseMenu.isOpen
      }
    }

    if (pauseMenu.ContinueButton.input.clicked) {
      pauseMenu.isOpen = false
    }

    if (pauseMenu.ReturnToMenuButton.input.clicked) {
      finished = true
      GameState.push(new MenuState())
    }

    inputs.update()

    val renderer = Renderer.get
    renderer.setWriteSrgb(true)
    renderer.setRenderTarget(RenderTarget.Backbuffer)
    // renderer.clear(Some(Color.Black), None)
    renderer.clear(Some(Color.rgb(0x6495ED)), None)

    pauseMenu.update(dt)

    canvas.render()
    LayoutDebugger.render()
    DebugDraw.render2D()

    renderer.endFrame()

    AppWindow.swapBuffers()
  }

  override def done: Boolean = finished

}

