package res.process

import res.intermediate._

/**
  * Resolve textures used by a material.
  */
object ResolveMaterial {

  /** Get the base filename without paths or extensions */
  private def baseName(filename: String): String = {
    var name = filename
    if (name.lastIndexOf('/') >= 0)  name = name.substring(name.lastIndexOf('/') + 1)
    if (name.lastIndexOf('\\') >= 0) name = name.substring(name.lastIndexOf('\\') + 1)
    if (name.indexOf('.') >= 0)      name = name.substring(0, name.indexOf('.'))
    name
  }

  /**
    * Resolve a material from a single texture name.
    *
    * @param texture Name of the diffuse texture used by a mesh.
    * @param siblingFiles All the files that are in the same folder as this file.
    * @param resolveOutputFile Get the final data name for a file in `siblingFiles`
    */
  def resolveMaterialFromTexture(texture: String, siblingFiles: Seq[String], resolveOutputFile: String => String): Material = {
    val baseTex = baseName(texture)

    def resolve(file: String): String = {
      val data = resolveOutputFile(file)
      data ++ ".s2tx"
    }

    val albedo = siblingFiles.find(n => baseName(n) == baseTex + "_albedo")
        .orElse(siblingFiles.find(n => baseName(n) == baseTex)).map(resolve).getOrElse("")
    val normal = siblingFiles.find(n => baseName(n) == baseTex + "_normal").map(resolve).getOrElse("")
    val roughness = siblingFiles.find(n => baseName(n) == baseTex + "_roughness").map(resolve).getOrElse("")
    val metallic = siblingFiles.find(n => baseName(n) == baseTex + "_metallic").map(resolve).getOrElse("")
    val ao = siblingFiles.find(n => baseName(n) == baseTex + "_ao").map(resolve).getOrElse("")

    Material(albedo, normal, roughness, metallic, ao)
  }

}

