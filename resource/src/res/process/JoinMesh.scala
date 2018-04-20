package res.process

import core._
import res.intermediate._
import res.intermediate.Mesh._
import res.intermediate.Model._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Join small mesh parts into a skinned mesh with one bone per part.
  */
object JoinMesh {

  def removeMeshFromNodes(node: ModelNode, name: String): Option[String] = {
    for (mesh <- node.meshes) {
      if (mesh.name == name) {
        node.meshes -= mesh
        return Some(node.name)
      }
    }

    for (child <- node.children) {
      for (res <- removeMeshFromNodes(child, name)) {
        return Some(res)
      }
    }

    None
  }

  def joinMeshPart(model: Model, name: String, material: Material, meshes: Seq[Mesh]): Mesh = {
    var bones = new ArrayBuffer[MeshBone]()
    var vertices = new ArrayBuffer[Vertex]()
    var indices = new ArrayBuffer[Int]()

    var baseIndex = 0
    for (mesh <- meshes) {
      val boneIx = bones.length

      val bone = new MeshBone()
      bone.name = removeMeshFromNodes(model.root, mesh.name).getOrElse {
        throw new RuntimeException(s"Mesh ${mesh.name} not found in model")
      }
      bone.meshToBone = Matrix43.Identity
      bones += bone

      for (ix <- mesh.indices) {
        indices += ix + baseIndex
      }
      baseIndex += mesh.vertices.length

      for (vert <- mesh.vertices) {
        assert(vert.bones.isEmpty)
        val copy = vert.copy
        copy.bones += BoneWeight(boneIx, 1.0)
        vertices += copy
      }

    }

    val result = new Mesh(name)
    result.material = material
    result.bones = bones
    result.vertices = vertices.toArray
    result.indices = indices.toArray
    result
  }

  def joinMeshParts(model: Model, name: String, meshes: Seq[Mesh]): Iterable[Mesh] = {
    val draws = meshes.groupBy(_.material)

    val result = for (((material, meshes), index) <- draws.zipWithIndex) yield {
      joinMeshPart(model, s"$name.$index", material, meshes)
    }

    result
  }

  def joinMeshes(model: Model, meshes: Seq[Mesh], config: Config.Res.Model): Seq[Mesh] = {
    def findMeshConfigs(name: String): Seq[Config.Res.Model.Mesh] = config.meshes.filter(cfg => {
      cfg.nameRegex match {
        case Some(regex) => regex.findFirstIn(name).isDefined
        case None => true
      }
    })

    val joinedMeshes = new mutable.HashMap[String, List[Mesh]]().withDefaultValue(Nil)

    for (mesh <- meshes) {
      for (name <- findMeshConfigs(mesh.name).map(_.joinMeshName).filter(_.nonEmpty).headOption) {
        assert(mesh.bones.isEmpty, s"Cannot join mesh with bones: ${mesh.name}")
        joinedMeshes(name) :+= mesh
      }
    }

    if (joinedMeshes.isEmpty) {
      meshes
    } else {
      val generatedMeshes = joinedMeshes.flatMap(pair => joinMeshParts(model, pair._1, pair._2)).toSeq

      for (mesh <- generatedMeshes) {
        model.root.meshes += ModelMesh(mesh.name)
      }

      generatedMeshes
    }
  }

}


