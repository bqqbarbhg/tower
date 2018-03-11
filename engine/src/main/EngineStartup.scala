package main

import platform.AppWindow
import render._
import render.opengl.{MapMode, OptsGl}

object EngineStartup {

  class Options {
    var debug: Boolean = false
    var glCompatability: Boolean = false
    var initialWidth: Int = 1280
    var initialHeight: Int = 720
    var windowName: String = ""
  }

  def start(opts: Options): Unit = {
    AppWindow.initialize(opts.initialWidth, opts.initialHeight, opts.windowName, opts.debug)

    val device = GraphicsDevice.get
    if (device.doesNotSupportRowMajor) {
      OptsGl.useRowMajorMatrix = false
    }

    if (opts.glCompatability) {
      OptsGl.vertexMap = MapMode.SubData
      OptsGl.uniformMap = MapMode.SubData
      OptsGl.useVaoCache = false
      OptsGl.useUniformBlocks = false
      OptsGl.useTexStorage = false
    }

    Renderer.initialize()
  }

}