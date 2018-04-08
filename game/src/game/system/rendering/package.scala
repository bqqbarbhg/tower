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

  def load(): Unit = {
    cullingSystem = new CullingSystemImpl()
    pointLightSystem = new PointLightSystemImpl()
    ambientSystem = new AmbientSystemImpl()
    ambientPointLightSystem = new AmbientPointLightSystemImpl()
    modelSystem = new ModelSystemImpl()
    forwardRenderingSystem = new ForwardRenderingSystemImpl()
    shadowRenderingSystem = new ShadowRenderingSystemImpl()

    // Dependency: ambientSystem
    groundSystem = new GroundSystemImpl()

    base.entitySystem.addDeleteListener(cullingSystem)
    base.entitySystem.addDeleteListener(pointLightSystem)
    base.entitySystem.addDeleteListener(ambientSystem)
    base.entitySystem.addDeleteListener(ambientPointLightSystem)
    base.entitySystem.addDeleteListener(modelSystem)
  }

}
