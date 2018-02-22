package io.content

import java.nio.file.Paths
import java.io.FileInputStream

import DirectoryPackage._

object DirectoryPackage {

  class DirectoryFile(val file: java.io.File) extends File {
    override def sizeInBytes: Long = file.length
    override def read() = new FileInputStream(file)
  }

}

/**
  * Package that serves files from some plain folder.
  */
class DirectoryPackage(val basePath: String) extends Package {

  private val absolutePath = new java.io.File(basePath).getAbsolutePath

  def name: String = s"dir:$basePath"

  def get(filename: String): Option[File] = {
    val file = Paths.get(basePath, filename).toFile
    if (!file.exists) return None
    if (file.isDirectory) return None
    Some(new DirectoryFile(file))
  }

  def list(path: String): Seq[FileInfo] = {
    def toFileInfo(file: java.io.File): FileInfo = {
      val path = file.getAbsolutePath.substring(absolutePath.length + 1).replace('\\', '/')
      FileInfo(path, file.isDirectory)
    }

    try {
      val files = Paths.get(basePath, path).toFile.listFiles
      if (files == null) return Array[FileInfo]()
      files.map(toFileInfo)
    } catch {
      case e: Exception => Array[FileInfo]()
    }
  }

}
