package game.system.rendering

import scala.collection.mutable.ArrayBuffer
import core._
import game.system.Entity
import PointLightSystem._
import PointLightSystemImpl._
import util.geometry.Sphere

import scala.collection.mutable

object PointLightSystem {

  val MaxObjectRadius = 10.0

  /**
    * A punctual point light attached to some entity.
    *
    * @param entity Owner of this point light
    * @param localPosition Position relative to the entity
    * @param intensity RGB intensity of the light
    * @param radius Maximum distance the light can have an effect
    */
  sealed abstract class PointLight(val entity: Entity, var localPosition: Vector3, var intensity: Vector3, var radius: Double) {

  }

  /**
    * An entity can use these to gather point lights.
    *
    * @param entity Owner of this receiver
    * @param sphere Local bounding sphere to receive light with
    */
  sealed abstract class PointLightReceiver(val entity: Entity, var sphere: Sphere) {

    /** PointLight instances that intersect with the receiver */
    var pointLights = new ArrayBuffer[PointLight]()

  }

}

sealed trait PointLightSystem {

  /**
    * Update receiving light probes for a list of visible entities.
    */
  def updateLightReceivers(renderableEntities: ArrayBuffer[Entity], lightEntities: ArrayBuffer[Entity]): Unit

  /** Add a point light. */
  def addLight(entity: Entity, localPosition: Vector3, intensity: Vector3, radius: Double, isStatic: Boolean): PointLight

  /** Add a receiver with a spherical bounding area. */
  def addReceiver(entity: Entity, sphere: Sphere, isStatic: Boolean): PointLightReceiver

  /** Remove all attached lights */
  def removeLights(entity: Entity): Unit

  /** Remove all attached lights */
  def removeReceivers(entity: Entity): Unit
}

object PointLightSystemImpl {

  final class PointLightImpl(entity: Entity, localPosition: Vector3, intensity: Vector3, radius: Double, val isStatic: Boolean)
    extends PointLight(entity, localPosition, intensity, radius) {

    /** Embedded linked list node */
    var next: PointLightImpl = null

    /** World-space bounding sphere */
    var worldSphere: Sphere = if (isStatic)
      Sphere(entity.position + localPosition, radius)
    else null
  }

  final class PointLightReceiverImpl(entity: Entity, sphere: Sphere, val isStatic: Boolean)
    extends PointLightReceiver(entity, sphere) {

    /** Embedded linked list node */
    var next: PointLightReceiverImpl = null

    /** World-space bounding sphere */
    var worldSphere: Sphere = if (isStatic)
      sphere.copy(center = sphere.center + entity.position)
    else null
  }

}

final class PointLightSystemImpl extends PointLightSystem {

  val entityToPointLight = new mutable.HashMap[Entity, PointLightImpl]()
  val entityToReceiver = new mutable.HashMap[Entity, PointLightReceiverImpl]()

  override def updateLightReceivers(renderableEntities: ArrayBuffer[Entity], lightEntities: ArrayBuffer[Entity]): Unit = {
    val lights = new ArrayBuffer[PointLightImpl]()
    val receivers = new ArrayBuffer[PointLightReceiverImpl]()

    // Gather point lights
    {
      var ix = 0
      val len = lightEntities.length
      while (ix < len) {
        val entity = lightEntities(ix)
        if ((entity.flag0 & Entity.Flag0_HasPointLight) != 0) {
          var pointLight = entityToPointLight(entity)
          do {
            lights += pointLight
            pointLight = pointLight.next
          } while (pointLight != null)
        }
        ix += 1
      }
    }

    // Gather receivers
    {
      var ix = 0
      val len = renderableEntities.length
      while (ix < len) {
        val entity = renderableEntities(ix)
        if ((entity.flag0 & Entity.Flag0_HasPointLightReceiver) != 0) {
          var receiver = entityToReceiver(entity)
          do {
            receivers += receiver
            receiver = receiver.next
          } while (receiver != null)
        }
        ix += 1
      }
    }

    // Terrible O(n^2) implementation
    {
      for (light <- lights) {
        if (!light.isStatic) {
          light.worldSphere = Sphere(light.entity.position + light.localPosition, light.radius)
        }
      }

      for (receiver <- receivers) {
        if (!receiver.isStatic) {
          val s = receiver.worldSphere
          receiver.worldSphere = s.copy(center = s.center + receiver.entity.position)
        }

        for (light <- lights) {
          if (receiver.worldSphere intersects light.worldSphere) {
            receiver.pointLights += light
          }
        }
      }
    }
  }

  override def addLight(entity: Entity, localPosition: Vector3, intensity: Vector3, radius: Double, isStatic: Boolean): PointLight = {
    val light = new PointLightImpl(entity, localPosition, intensity, radius, isStatic)

    light.next = if ((entity.flag0 & Entity.Flag0_HasPointLight) != 0)
      entityToPointLight(entity)
    else null

    entity.flag0 |= Entity.Flag0_HasPointLight

    light
  }

  override def addReceiver(entity: Entity, sphere: Sphere, isStatic: Boolean): PointLightReceiver = {
    val receiver = new PointLightReceiverImpl(entity, sphere, isStatic)

    receiver.next = if ((entity.flag0 & Entity.Flag0_HasPointLightReceiver) != 0)
      entityToReceiver(entity)
    else null

    entity.flag0 |= Entity.Flag0_HasPointLightReceiver

    receiver
  }

  override def removeLights(entity: Entity): Unit = {
    entityToPointLight.remove(entity)
  }

  override def removeReceivers(entity: Entity): Unit = {
    entityToReceiver.remove(entity)
  }
}

