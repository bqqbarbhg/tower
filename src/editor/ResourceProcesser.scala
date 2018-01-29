package editor

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import authoring.Asset
import authoring.processing.AnimationOptimization
import authoring.resource._

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

          res match {
            case a: Animation =>
              a.timelines = a.timelines.map(timeline => {
                println(s"  .. Optimizing timeline: ${timeline.boneName}")
                val error = 0.01
                val rot = AnimationOptimization.reduceRotationKeyframes(timeline, 0.01)
                println(s"  .... Reduced rotation keyframes: ${timeline.rot.length} -> ${rot.rot.length} (max error ${error})")

                rot
              })

            case _ =>
          }
        }
      case None =>
    }
  }

}
