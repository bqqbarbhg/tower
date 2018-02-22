package res.importer

import org.lwjgl.system.MemoryUtil
import res.intermediate._
import core._
import util.BufferUtils._

import scala.io.Source

object GlslImporter extends Importer {
  override def importType: ImportFileType = ImportFileShader

  def importAsset(asset: AssetFile): Iterable[Resource] = {
    val source = Source.fromFile(asset.file, "UTF-8").mkString
    Some(new Shader(source))
  }
}

