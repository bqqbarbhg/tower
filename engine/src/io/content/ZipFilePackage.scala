package io.content

import java.util.zip.{ZipEntry, ZipFile}

import scala.collection.JavaConverters._
import scala.collection.mutable.{ArrayBuffer, HashMap}

import ZipFilePackage._

object ZipFilePackage {

  class ZipPackageFile(val entry: ZipEntry, val file: ZipFile) extends File {
    override def sizeInBytes: Long = entry.getSize
    override def read() = file.getInputStream(entry)
  }

}

/**
  * Package that is served from an external .zip -file.
  */
class ZipFilePackage(val path: String) extends Package {

  private val zipFile = new ZipFile(new java.io.File(path), ZipFile.OPEN_READ)

  private val folders = new HashMap[String, ArrayBuffer[FileInfo]]()
  private val entries = new HashMap[String, ZipEntry]()

  {
    val it = zipFile.entries()
    while (it.hasMoreElements) {
      val entry = it.nextElement()
      var name = entry.getName
      if (name.endsWith("/"))
        name = name.substring(0, name.length - 1)

      val parent = if (name.contains('/'))
        name.substring(0, name.lastIndexOf('/'))
      else
        ""

      val folder = folders.getOrElseUpdate(parent, ArrayBuffer[FileInfo]())
      folder += FileInfo(name, entry.isDirectory)
      entries(name) = entry
    }
  }

  def name: String = s"zip:$path"
  def get(path: String): Option[File] = entries.get(path).map(entry => new ZipPackageFile(entry, zipFile))
  def list(path: String): Seq[FileInfo] = folders.get(path).getOrElse(Array[FileInfo]())

}
