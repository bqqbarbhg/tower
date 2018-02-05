package tower.editor

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import tower.authoring.Asset
import tower.authoring.output._
import tower.authoring.processing._
import tower.authoring.resource._

import scala.reflect.io.Path

object ResourceProcesser extends App {

  println("Listing resources...")

  def listFilesRecursive(root: File): Seq[File] = {
    if (!root.exists) return None.toSeq
    root.listFiles.flatMap(file => {
      if (file.isDirectory) listFilesRecursive(file)
      else Some(file)
    })
  }

  val assetRoot = new File("asset")
  val assetRootPath = assetRoot.getAbsolutePath

  val dataRoot = new File("data")
  val dataRootPath = dataRoot.getAbsolutePath

  for (file <- listFilesRecursive(assetRoot)) {
    val relativeFilename = file.getAbsolutePath.substring(assetRootPath.length + 1)
    val baseName = if (relativeFilename.contains('.')) {
      relativeFilename.substring(0, relativeFilename.lastIndexOf('.'))
    } else {
      relativeFilename
    }

    Asset.deferLoad(file.getAbsolutePath, baseName) match {
      case Some(loader) =>

        println(s"Found asset: ${relativeFilename}")
        val asset = loader()
        for (res <- asset.resources) {
          println(s"  Resource: ${res.name} (${res.getClass.getName})")

          res match {
            case a: AnimationResource =>
              a.timelines = a.timelines.map(timeline => {
                println(s"  .. Optimizing timeline: ${timeline.boneName}")
                val error = 0.01
                val rot = AnimationOptimization.reduceRotationKeyframes(timeline, 0.005)
                println(s"  .... Reduced rotation keyframes: ${timeline.rot.length} -> ${rot.rot.length} (max error ${0.005})")
                val pos = AnimationOptimization.reducePositionKeyframes(rot, 0.1)
                println(s"  .... Reduced position keyframes: ${rot.pos.length} -> ${pos.pos.length} (max error ${0.1})")
                val siz = AnimationOptimization.reduceSizeKeyframes(pos, 0.05)
                println(s"  .... Reduced size     keyframes: ${rot.pos.length} -> ${pos.pos.length} (max error ${0.05})")

                siz
              })

              val file = Paths.get(dataRootPath, a.name).toFile
              file.getParentFile.mkdirs()
              AnimationFile.save(file.getAbsolutePath, a)

            case m: MeshResource =>
              val MaxBonesPerVert = 4
              val MaxBonesPerDraw = 12

              MeshProcessing.sortBoneWeights(m)
              MeshOptimization.limitBoneAmountPerVertex(m, MaxBonesPerVert)
              MeshProcessing.normalizeBoneWeights(m)

              val parts = MeshOptimization.splitMeshByBoneAmount(m, MaxBonesPerDraw)
              println(s"  Parts: ${parts.length}")
              for ((part, index) <- parts.zipWithIndex) {
                println(s"  Part $index")
                println(s"    Vertices: ${part.vertices.length}")
                println(s"    Indices:  ${part.indices.length}")
                println(s"    Bones:  ${part.bones.length}")
              }

              val file = Paths.get(dataRootPath, m.name).toFile
              file.getParentFile.mkdirs()
              MeshFile.save(file.getAbsolutePath, m.name, parts)

            case m: ModelResource =>

              def printNode(node: ModelNode, indent: Int = 0): Unit = {
                val ind = "    " + " " * indent
                println(ind + node.name)
                for (mesh <- node.meshes) {
                  println(ind + " Mesh: " + mesh.meshResource)
                }
                for (child <- node.children) {
                  printNode(child, indent + 1)
                }
              }

              for (anim <- m.animationResources)
                println(s"    Animation: $anim")

              printNode(m.root)

              val file = Paths.get(dataRootPath, m.name).toFile
              file.getParentFile.mkdirs()
              ModelFile.save(file.getAbsolutePath, m)

            case img: ImageResource =>
              println(s"${img.width}x${img.height}")

              val levels = MipmapGeneration.generateMipmaps(img)

              val file = Paths.get(dataRootPath, img.name).toFile
              file.getParentFile.mkdirs()

              val levelsDxt = for (level <- levels) yield DxtCompression.compressDxt(level)
              TextureFile.save(file.getAbsolutePath, levelsDxt)

            case _ =>
          }
        }
      case None =>
    }
  }

}
