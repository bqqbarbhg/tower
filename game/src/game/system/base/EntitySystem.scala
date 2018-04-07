package game.system.base

import game.system.Entity
import game.system.rendering._

sealed trait EntitySystem {

  /** Remove an entity from the world */
  def delete(entity: Entity): Unit

}

final class EntitySystemImpl extends EntitySystem {

  def delete(entity: Entity): Unit = {
    if ((entity.flag0 & Entity.Flag0_HasModel) != 0) modelSystem.removeModels(entity)
    if ((entity.flag0 & Entity.Flag0_HasPointLight) != 0) pointLightSystem.removeLights(entity)
    if ((entity.flag0 & Entity.Flag0_HasPointLightReceiver) != 0) pointLightSystem.removeReceivers(entity)
    if ((entity.flag0 & Entity.Flag0_HasCullables) != 0) cullingSystem.removeEntity(entity)
    if ((entity.flag0 & Entity.Flag0_HasAmbientProbes) != 0) ambientSystem.removeProbesFrom(entity)
    if ((entity.flag0 & Entity.Flag0_HasAmbientPointLight) != 0) ambientPointLightSystem.removeLights(entity)
  }

}

