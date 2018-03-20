package res.process

import res.intermediate._

/** Override model configuration stuff from config files */
object PatchModelProperties {


  def patchModelProperties(meshes: Seq[Mesh], config: Config.Res.Model): Unit = {

    def findMeshConfigs(name: String): Seq[Config.Res.Model.Mesh] = config.meshes.filter(cfg => {
      cfg.nameRegex match {
        case Some(regex) => regex.findFirstIn(name).isDefined
        case None => true
      }
    })

    for (mesh <- meshes) {
      val configs = findMeshConfigs(mesh.name)
      for (config <- configs) {
        if (config.texture.nonEmpty) {
          mesh.textureName = config.texture
        }
      }
    }

  }

}

