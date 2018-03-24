package main

import asset.AssetLoader
import core.StackAllocator
import game.state._
import game.system.RenderingSystem
import platform.AppWindow
import io.content._

object EditorMain extends App {

  // Initialize 64MB stack
  StackAllocator.createCurrentThread(64 * 1024 * 1024)

  val arg = util.ArgumentParser.parse(args,
    implicitArgumentFlags = Vector("process"),
    multiArgumentFlags = Vector("process"),
    aliases = Map("P" -> "process"))

  val process = arg.multiKeywords("process")
  if (process.nonEmpty) {
    import res.runner.{RunOptions, Runner}
    val opts = RunOptions.createFromFiles(process)
    val runner = new Runner(opts)
    runner.run()
  }

  val pack = new MultiPackage()
  JarPackage.create("data") match {
    case Some(jar) => pack.add(jar, 1)
    case None => // Nop
  }

  pack.add(new DirectoryPackage("data"), 0)

  Package.set(pack)

  AssetLoader.preloadAtlases()

  val opts = new GameStartup.Options()
  opts.engine.debug = arg.flag("debug")
  opts.engine.glCompatability = true
  GameStartup.start(opts)

  do {
    GameStartup.restartRequested = false

    GameState.push(new MenuState())

    while (AppWindow.running && !GameStartup.restartRequested) {
      RenderingSystem.updateScreenSize(AppWindow.width, AppWindow.height)
      GameState.update()
    }

    if (GameStartup.restartRequested) {
      GameStartup.restart()
    }

  } while (GameStartup.restartRequested)

  GameStartup.stop()
}
