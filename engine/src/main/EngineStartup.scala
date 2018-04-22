package main

import java.io.FileNotFoundException

import core._
import util.BufferUtils._
import asset.AssetLoader
import debug.ResourceTracker
import locale.LocaleInfo
import platform.AppWindow
import render._
import render.opengl.{MapMode, OptsGl}
import task.Task
import util.BufferIntegrityException

object EngineStartup {

  class Options {
    var debug: Boolean = false
    var profile: Boolean = false
    var glCompatability: Boolean = false
    var initialWidth: Int = 1280
    var initialHeight: Int = 720
    var numWorkers: Int = 0
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

  var workers: Array[WorkerThread] = Array[WorkerThread]()

  class WorkerThread extends Thread {
    override def run(): Unit = {
      val executor = Task.Worker

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

  def verifyDataRoot(): Option[Exception] = withStack {
    val pack = io.content.Package.get
    val file = pack.get("data_root.s2dr")

    try {
      file match {
        case Some(f) =>
          val stream = f.read()
          val buffer = alloca(1024)
          buffer.readFrom(stream)
          stream.close()
          buffer.finish()

          val MaxVersion = 1
          buffer.verifyMagic("s2dr")
          buffer.getVersion(MaxVersion)

          buffer.verifyMagic("E.dr")

          None

        case None => Some(new FileNotFoundException("Data root file 'data_root.s2dr' not found"))
      }
    } catch {
      case ex: BufferIntegrityException => Some(ex)
    }
  }

  def start(opts: Options): Unit = {
    AppWindow.initialize()

    IoThread.setName("Engine IO thread")
    IoThread.start()
    Task.Main.claimForThisThread()

    val numWorkers = if (opts.numWorkers > 0) {
      opts.numWorkers
    } else {
      math.min(Runtime.getRuntime.availableProcessors, 6)
    }

    workers = Array.tabulate(numWorkers)(i => {
      val thread = new WorkerThread()
      thread.setName(s"Engine Worker $i")
      thread.start()
      thread
    })

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

    for (worker <- workers) {
      worker.interrupt()
      worker.join()
    }
  }
}
