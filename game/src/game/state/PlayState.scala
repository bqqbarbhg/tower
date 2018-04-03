package game.state

import core._
import render._
import ui._
import platform.AppWindow

class PlayState extends GameState {

  override def load(): Unit = {
  }
  override def start(): Unit = {
  }
  override def stop(): Unit = {
  }

  override def update(): Unit = {
    AppWindow.pollEvents()
    val renderer = Renderer.get


    LayoutDebugger.render()

    renderer.endFrame()

    AppWindow.swapBuffers()
  }

  override def done: Boolean = false

}

