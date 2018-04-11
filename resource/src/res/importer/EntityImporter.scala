package res.importer

import org.lwjgl.system.MemoryUtil
import res.intermediate._
import core._
import io.Toml
import util.BufferUtils._

import scala.io.Source

object EntityImporter extends Importer {
  override def importType: ImportFileType = ImportFileEntity

  def importAsset(asset: AssetFile): Iterable[Resource] = {
    val root = Toml.parseFile(asset.file.getAbsolutePath)
    Some(new EntitySpec(root))
  }

}
