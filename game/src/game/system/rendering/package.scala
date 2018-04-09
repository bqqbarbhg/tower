package game.system

package object rendering {

  var cullingSystem: CullingSystem = null
  var pointLightSystem: PointLightSystem = null
  var ambientSystem: AmbientSystem = null
  var groundSystem: GroundSystem = null
  var ambientPointLightSystem: AmbientPointLightSystem = null
  var modelSystem: ModelSystem = null
  var forwardRenderingSystem: ForwardRenderingSystem = null
  var shadowRenderingSystem: ShadowRenderingSystem = null
  var cableRenderSystem: CableRenderSystem = null
  var globalRenderSystem: GlobalRenderSystem = null

  def loadGlobal(): Unit = {
    globalRenderSystem = new GlobalRenderSystemImpl()
    forwardRenderingSystem = new ForwardRenderingSystemImpl()
    shadowRenderingSystem = new ShadowRenderingSystemImpl()
  }

  def loadState(): Unit = {
    modelSystem = new ModelSystemImpl()
    cullingSystem = new CullingSystemImpl()
    pointLightSystem = new PointLightSystemImpl()
    ambientSystem = new AmbientSystemImpl()
    ambientPointLightSystem = new AmbientPointLightSystemImpl()

    base.entitySystem.addDeleteListener(cullingSystem)
    base.entitySystem.addDeleteListener(pointLightSystem)
    base.entitySystem.addDeleteListener(ambientSystem)
    base.entitySystem.addDeleteListener(ambientPointLightSystem)
    base.entitySystem.addDeleteListener(modelSystem)
  }

  def loadGame(): Unit = {
    groundSystem = new GroundSystemImpl()
    cableRenderSystem = new CableRenderSystemImpl()

    groundSystem.createGroundPlates()

    base.entitySystem.addDeleteListener(cableRenderSystem)
  }

  def unloadGame(): Unit = {
    base.entitySystem.removeDeleteListener(cableRenderSystem)

    groundSystem.unload()
    cableRenderSystem.unload()
  }

  def unloadState(): Unit = {
  }

  def unloadGlobal(): Unit = {
    globalRenderSystem.unload()
  }

}
