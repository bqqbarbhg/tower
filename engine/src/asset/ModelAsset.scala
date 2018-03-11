package asset

import core._
import gfx._
import ModelAsset._

object ModelAsset {
  val TexturesPerMaterial = 2

  def apply(name: String): ModelAsset = apply(Identifier(name))
  def apply(name: Identifier): ModelAsset = AssetLoader.getOrAdd(name, new ModelAsset(name))
}

class ModelAsset(val name: Identifier) extends LoadableAsset {

  private var modelImpl: Model = null
  private var meshes = Array[MeshAsset]()
  private var animations = Array[AnimationAsset]()
  private var materialTextures = Array[TextureAsset]()

  def get: Model = {
    load()
    modelImpl
  }

  override def preloadAsset(): Iterable[LoadableAsset] = {
    // @Todo: What to do about failed loads?
    modelImpl = Model.load(name).get

    meshes = modelImpl.meshResource.map(m => new MeshAsset(new Identifier(m))).toArray

    val materialIds = modelImpl.materials.flatMap(mat => {
      Vector(mat.albedoTexRes, mat.normalTexRes)
    })
    assert(materialIds.length == modelImpl.numMaterials * TexturesPerMaterial)
    materialTextures = materialIds.map(id => {
      if (id == Identifier.Empty) return null
      TextureAsset(id)
    })

    animations = modelImpl.animResource.map(m => new AnimationAsset(new Identifier(m))).toArray

    meshes ++ materialTextures ++ animations
  }

  override def loadAsset(): Unit = {
    for (i <- 0 until modelImpl.numMeshes) {
      modelImpl.meshes(i) = meshes(i).get
      modelImpl.meshes(i).material = modelImpl.materials(modelImpl.meshMaterialIndex(i))
    }

    for (i <- 0 until modelImpl.numMaterials) {
      val base = i * TexturesPerMaterial
      val mat = modelImpl.materials(i)

      mat.albedoTex = Option(materialTextures(base + 0)).map(_.get).getOrElse(Material.missingAlbedo)
      mat.normalTex = Option(materialTextures(base + 1)).map(_.get).getOrElse(Material.missingNormal)
    }

    for (i <- 0 until modelImpl.numAnims) {
      modelImpl.anims(i) = animations(i).get
    }
  }

  override def unloadAsset(): Unit = {
    modelImpl.unload()
    modelImpl = null
    meshes = Array[MeshAsset]()
    materialTextures = Array[TextureAsset]()
  }
}
