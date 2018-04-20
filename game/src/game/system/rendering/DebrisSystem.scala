package game.system.rendering

import core._
import asset._
import game.system._
import game.system.Entity._
import game.component.DebrisComponent
import DebrisSystemImpl._
import game.options.Options
import game.system.rendering.ModelSystem.NodeInstance
import util.geometry._

import scala.collection.mutable
import scala.util.Random

sealed trait DebrisSystem extends EntityDeleteListener {

  /** Update all debris effects */
  def update(dt: Double): Unit

  /** Update visible debris effects */
  def updateVisible(dt: Double, visible: EntitySet): Unit

  /** Add a debris burst effect */
  def addDebrisBurst(entity: Entity, component: DebrisComponent): Entity

}

object DebrisSystemImpl {

  class DebrisPart(val worldToPose: Matrix43, val node: NodeInstance, var velocity: Vector3, var angularVel: Quaternion) {
    val initialAffine = worldToPose.toAffine

    var position = initialAffine.position
    var scale = initialAffine.scale
    var rotation = initialAffine.rotation

    val inversePose = worldToPose.inverse

    var unsafeTransform: Matrix43 = new Matrix43.Unsafe()
  }

  class DebrisInstance(val parent: Entity, val component: DebrisComponent, val hasShadow: Boolean) {
    val system = debrisSystem.asInstanceOf[DebrisSystemImpl]
    val random = system.sharedRandom

    val entity = new Entity(true, "Debris")
    entity.position = parent.position
    entity.rotation = parent.rotation

    cullingSystem.addSphere(entity, Sphere(Vector3.Zero, component.cullRadius), {
      var mask = CullingSystem.MaskRender
      if (hasShadow) mask |= CullingSystem.MaskShadow
      mask
    })

    val model = modelSystem.addModel(entity, component.model)
    val nodes = model.model.nodeName.flatMap(nameIx => {
      val name = new Identifier(nameIx)
      if (name.toString.startsWith(component.partPrefix)) {
        model.findNode(name)
      } else {
        None
      }
    })

    val center = entity.transformPoint(component.center)

    val parts = nodes.map(node => {
      val nodeInParent = model.model.transformToRoot(node.index)
      val parentInWorld = entity.worldTransform
      val nodeInWorld = parentInWorld * nodeInParent

      val pos = nodeInWorld.toAffine.position
      val baseVel = (pos - center).normalizeOrZero *@ component.baseVelocity
      val randomVel = Vector3(
        random.nextDouble() * 2.0 - 1.0,
        random.nextDouble() * 2.0 - 1.0,
        random.nextDouble() * 2.0 - 1.0) *@ component.randomVelocity

      val angularVel = Quaternion(
        random.nextDouble() * 2.0 - 1.0,
        random.nextDouble() * 2.0 - 1.0,
        random.nextDouble() * 2.0 - 1.0,
        0.0
      ) * component.rotationAmount

      val vel = component.globalVelocity + baseVel + randomVel

      new DebrisPart(nodeInWorld, node, vel, angularVel)
    })

    var time: Double = 0.0

    def update(dt: Double): Unit = {
      val scaleT = math.min((component.lifetime - time) / component.fadeTime, 1.0)
      val scale = smoothStep(scaleT)

      for (part <- parts) {
        part.velocity += component.gravity * dt
        part.position += part.velocity * dt
        part.rotation = (part.rotation + (part.angularVel * (0.5 * dt)) * part.rotation).normalize
        part.scale = part.initialAffine.scale * scale
      }

      time += dt
      if (time >= component.lifetime) {
        entity.delete()
      }
    }

    def updateVisible(dt: Double): Unit = {
      val system = debrisSystem.asInstanceOf[DebrisSystemImpl]
      val temp = system.tempMatrix

      for (part <- parts) {
        Matrix43.unsafeAffine(temp, AffineTransform(part.position, part.scale, part.rotation))
        part.unsafeTransform.unsafeMul(part.inversePose, temp)
        part.node.localTransform = part.unsafeTransform
      }
    }
  }

}

final class DebrisSystemImpl extends DebrisSystem {

  val sharedRandom = new Random()
  val useShadow = Options.current.graphics.quality.shaderQuality >= 2
  val activeDebris = new mutable.HashMap[Entity, DebrisInstance]()

  val tempMatrix = new Matrix43.Unsafe()

  override def update(dt: Double): Unit = {
    for (debris <- activeDebris.valuesIterator) {
      debris.update(dt)
    }
  }

  override def updateVisible(dt: Double, visible: EntitySet): Unit = {
    for (entity <- visible.flag(Flag_Debris); debris <- activeDebris.get(entity)) {
      debris.updateVisible(dt)
    }
  }

  override def addDebrisBurst(entity: Entity, component: DebrisComponent) = {
    val debris = new DebrisInstance(entity, component, useShadow)
    activeDebris(debris.entity) = debris
    debris.entity.setFlag(Flag_Debris)
    debris.entity
  }

  override def entitiesDeleted(entities: EntitySet): Unit = {
    for (e <- entities.flag(Flag_Debris)) {
      activeDebris.remove(e)
    }
  }
}

