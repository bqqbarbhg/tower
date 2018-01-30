package tower.game

import tower.engine.file.{DirectoryPackage, JarPackage, MultiPackage, ZipFilePackage}

object GameMain extends App {

  val pack = new MultiPackage()
  pack.add(new DirectoryPackage("data"), 0)
  pack.add(new ZipFilePackage("mods/wat.zip"), 2)

  JarPackage.create("data") match {
    case Some(jar) => pack.add(jar, 1)
    case None => // Nop
  }

  println(pack.list("wat").mkString("\n"))

}
