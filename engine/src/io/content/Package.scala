package io.content

import core._

/** Info about a file in a directory listing */
case class FileInfo(name: String, directory: Boolean)

/**
  * File handle that can be used to read the file
  */
trait File {

  /** Returns the file size in bytes */
  def sizeInBytes: Long

  /** Opens the file for reading */
  def read(): java.io.InputStream
}

object Package {
  private var instance: Package = null

  /** Setup the package */
  def set(value: Package) = {
    instance = value
  }

  /** Get the main package singleton */
  def get: Package = instance

}

/**
  * Represents an abstract read-only 'virtual filesystem' that can serve game content files.
  */
trait Package {

  /** Returns an unique, but stable name for the package that can be used for sorting */
  def name: String

  /** Get a file descriptor for a filename */
  def get(filename: String): Option[File]

  /** Get a file descriptor for a filename using an identifier */
  def get(filename: Identifier): Option[File] = get(filename.toString)

  /** List all the files and directories in a path */
  def list(path: String): Seq[FileInfo]

}
