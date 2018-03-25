package main

import asset.AssetLoader
import debug.ResourceTracker
import locale.LocaleInfo
import platform.AppWindow
import render._
import render.opengl.{MapMode, OptsGl}
import task.Task

object EngineStartup {

  class Options {
    var debug: Boolean = false
    var profile: Boolean = false
    var glCompatability: Boolean = false
    var initialWidth: Int = 1280
    var initialHeight: Int = 720
    var windowName: String = ""
  }

  object IoThread extends Thread {
    override def run(): Unit = {
      val executor = Task.Io
      executor.claimForThisThread()

      try {
        while (!Thread.interrupted()) {
          executor.runNextWait()
        }
      } catch {
        case e: InterruptedException => // Nop
      }
    }
  }

  var launchOptions: Options = null

  def start(opts: Options): Unit = {
    AppWindow.initialize()

    IoThread.setName("Engine IO thread")
    IoThread.start()
    Task.Main.claimForThisThread()

    LocaleInfo.load()

    launchOptions = opts
  }

  def softStart(windowStyle: AppWindow.WindowStyle): Unit = {
    val opts = launchOptions

    AppWindow.recreateWindow(windowStyle, opts.windowName, opts.debug)

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

    if (opts.profile) {
      OptsGl.useProfiling = true
    }

    AppWindow.setSwapInterval(OptsGl.swapInterval)

    Renderer.initialize()
  }

  def softStop(): Unit = {
    AssetLoader.unloadEverything()
    Renderer.shutdown()

    val leaked = ResourceTracker.active
    if (leaked.length > 0) {
      println(s"Leaked ${leaked.length} resources:")
      for (res <- leaked) {
        println(s"-- ${res.kind} ${res.name}")
        for (f <- res.stack) {
          println(s"${f.getFileName}:${f.getLineNumber}: ${f.getClassName}.${f.getMethodName}")
        }
        println()
      }
    }
  }

  def stop(): Unit = {
    AppWindow.destroyWindow()
    AppWindow.unload()
    IoThread.interrupt()
    IoThread.join()
  }
}
