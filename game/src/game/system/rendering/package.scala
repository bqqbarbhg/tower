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
  var animationSystem: AnimationSystem = null

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
    animationSystem = new AnimationSystemImpl()

    base.entitySystem.addDeleteListener(cullingSystem)
    base.entitySystem.addDeleteListener(pointLightSystem)
    base.entitySystem.addDeleteListener(ambientSystem)
    base.entitySystem.addDeleteListener(ambientPointLightSystem)
    base.entitySystem.addDeleteListener(modelSystem)
    base.entitySystem.addDeleteListener(animationSystem)
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
    base.entitySystem.removeDeleteListener(cullingSystem)
    base.entitySystem.removeDeleteListener(pointLightSystem)
    base.entitySystem.removeDeleteListener(ambientSystem)
    base.entitySystem.removeDeleteListener(ambientPointLightSystem)
    base.entitySystem.removeDeleteListener(modelSystem)
    base.entitySystem.removeDeleteListener(animationSystem)
  }

  def unloadGlobal(): Unit = {
    globalRenderSystem.unload()
  }

}
