package io.content

import java.nio.file._
import java.util.Collections

import scala.collection.JavaConverters._

import JarPackage._

object JarPackage {

  def create(folder: String): Option[JarPackage] = {
    val path = "/resources/" + folder
    val resource = getClass.getResource(path)
    if (resource == null) return None
    val resourceUri = resource.toURI
    if (resourceUri.getScheme != "jar") return None

    val fs = FileSystems.newFileSystem(resourceUri, Collections.emptyMap[String, Object])
    val pack = new JarPackage(fs.getPath(path), folder)

    Some(pack)
  }

  class JarFile(val path: Path) extends File {
    override def sizeInBytes: Long = Files.size(path)
    override def read() = Files.newInputStream(path)
  }

}

/**
  * Package that is served directly from an internal .jar archive.
  */
class JarPackage(val rootPath: Path, val folder: String) extends Package {

  def name: String = s"jar:$folder"

  def get(path: String): Option[File] = {
    val jarPath = rootPath.resolve(path)
    if (!Files.exists(jarPath)) return None
    if (Files.isDirectory(jarPath)) return None
    Some(new JarFile(jarPath))
  }

  def list(path: String): Seq[FileInfo] = {

    def toFileInfo(path: Path): FileInfo = {
      val relative = rootPath.relativize(path).normalize.toString
      FileInfo(relative, Files.isDirectory(path))
    }

    try {
      val folder = rootPath.resolve(path)
      val files: Array[Path] = Files.walk(folder, 1).iterator.asScala.toArray
      files.map(toFileInfo)
    } catch {
      case e: Exception => Array[FileInfo]()
    }
  }

}

