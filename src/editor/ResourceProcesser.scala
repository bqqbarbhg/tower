package editor

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import authoring.Asset

object ResourceProcesser extends App {

  println("Listing resources...")

  def listFilesRecursive(root: File): Seq[File] = {
    if (!root.exists) return None.toSeq
    root.listFiles.flatMap(file => {
      if (file.isDirectory) listFilesRecursive(file)
      else Some(file)
    })
  }

  for (file <- listFilesRecursive(new File("asset"))) {
    Asset.deferLoad(file.getAbsolutePath) match {
      case Some(loader) =>
        println(s"Found asset: ${file}")
        val asset = loader()
        for (res <- asset.resources) {
          println(s"  Resource: ${res.name} (${res.getClass.getName})")
        }
      case None =>
    }
  }

}
