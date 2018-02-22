package io.content

import scala.collection.immutable.Set

import MultiPackage._

object MultiPackage {

  case class OrderedPackage(pack: Package, order: Int)

}

/** Combines multiple packages into a single one */
class MultiPackage extends Package {

  def name: String = "multi"

  private var packages = Vector[OrderedPackage]()

  /**
    * Add a new package to load files from
    * @param pack Package instance
    * @param order Sort order, smaller orders have priority over largers
    */
  def add(pack: Package, order: Int): Unit = {
    val ordered = OrderedPackage(pack, order)
    packages = (packages :+ ordered).sortBy(p => (p.order, p.pack.name))
  }

  /**
    * Remove a package from the meta-package
    * @param pack Package that has been added with `add` before
    */
  def remove(pack: Package): Unit = {
    packages = packages.filterNot(p => p.pack == pack)
  }

  def get(path: String): Option[File] = {
    for (pack <- packages) {
      pack.pack.get(path) match {
        case Some(file) => return Some(file)
        case None => // Nop
      }
    }
    None
  }
  def list(path: String): Seq[FileInfo] = {
    val results: Seq[FileInfo] = packages.flatMap(_.pack.list(path))
    val set: Set[FileInfo] = results.toSet
    set.toSeq
  }
}
