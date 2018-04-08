package game.system.rendering

import scala.collection.mutable
import core._
import game.system._
import game.system.Entity._
import game.lighting.LightProbe
import game.system.rendering.AmbientSystem._
import game.system.rendering.AmbientSystemImpl._

import scala.collection.mutable.ArrayBuffer

object AmbientSystem {

  /**
    * A punctual probe that can receive ambient light.
    *
    * @param entity Entity attached to, may be `null` if global.
    */
  sealed abstract class Probe(val entity: Entity) {

    /** LightProbe containing environment irradiance, ie. incoming light to a
      * hemisphere per direction. */
    val irradianceProbe: LightProbe = LightProbe.make()

    /** Position relative to `entity` or world origin if `entity == null` */
    var localPosition: Vector3 = Vector3.Zero

    /** Position in the world (valid only after update) */
    var worldPosition: Vector3 = Vector3.Zero
  }

  /** Empty light probe that can be used as a placeholder. */
  val NullProbe: Probe = {
    val p = new ProbeImpl(null)
    // If `visibleMark` is always set it will never be updated
    p.visibleMark = true
    p
  }

}

sealed trait AmbientSystem extends EntityDeleteListener {

  /**
    * Create a new probe with an absolute position.
    */
  def createGlobalProbe(position: Vector3): Probe

  /**
    * Create a new probe that can act as an indirect light emitter.
    */
  def createGlobalProbeForIndirect(position: Vector3): Probe

  /**
    * Create an ambient probe that is attached to `entity`'s local coordinates.
    */
  def addProbe(entity: Entity, localPosition: Vector3): Probe

  /**
    * Add a dependency to `probe` from `entity`. If `entity` is visible then
    * `probe` will be updated.
    */
  def addProbeDependency(entity: Entity, probe: Probe): Unit

  /**
    * Remove an entity with probes from the system.
    */
  def removeProbes(entity: Entity): Unit

  /**
    * Collect and update light probes that are visible.
    *
    * @param visible Set of visible entities
    */
  def updateVisibleProbes(visible: EntitySet): ArrayBuffer[Probe]

  /**
    * Add indirect lighting to visible probes.
    */
  def updateIndirectLight(probes: ArrayBuffer[Probe]): Unit

  /**
    * Clean-up after all the frame processing has been done.
    */
  def frameCleanup(visibleProbes: ArrayBuffer[Probe]): Unit

  /** Default light probe base for everything */
  def globalProbe: LightProbe

}

object AmbientSystemImpl {

  final class ProbeImpl(entity: Entity) extends Probe(entity) {
    val static: Boolean = entity == null || entity.static
    var indirectProbes = Array[Probe]()
    var indirectWeights = Array[Double]()
    var visibleMark: Boolean = false

    var isIndirectEmitter: Boolean = false
    var cachedIndirectLight: Vector3 = Vector3.Zero
  }

}

final class AmbientSystemImpl extends AmbientSystem {

  val entityToProbe = new mutable.HashMap[Entity, Array[ProbeImpl]]().withDefaultValue(Array[ProbeImpl]())

  var visibleProbes = new ArrayBuffer[Probe]()

  override val globalProbe = LightProbe.make()

  private def linkIndirectProbes(probe: ProbeImpl): Unit = {
    groundSystem.getProbesAndWeights(probe.worldPosition, probe.indirectProbes, probe.indirectWeights)
  }

  override def createGlobalProbe(position: Vector3): Probe = {
    val probe = new ProbeImpl(null)
    probe.localPosition = position
    probe.worldPosition = position
    probe.indirectProbes = new Array[Probe](4)
    probe.indirectWeights = new Array[Double](4)
    linkIndirectProbes(probe)
    probe
  }

  override def createGlobalProbeForIndirect(position: Vector3): Probe = {
    val probe = new ProbeImpl(null)
    probe.localPosition = position
    probe.worldPosition = position
    probe.isIndirectEmitter = true
    probe
  }

  override def addProbe(entity: Entity, localPosition: Vector3): Probe = {
    val probe = new ProbeImpl(entity)
    probe.localPosition = localPosition
    probe.indirectProbes = new Array[Probe](4)
    probe.indirectWeights = new Array[Double](4)

    if (entity.static) {
      probe.worldPosition = entity.position + localPosition
      linkIndirectProbes(probe)
    }

    addProbeDependency(entity, probe)

    probe
  }

  override def addProbeDependency(entity: Entity, probe: Probe): Unit = {
    entityToProbe(entity) :+= probe.asInstanceOf[ProbeImpl]
    entity.setFlag(Flag_AmbientProbes)
  }

  override def removeProbes(entity: Entity): Unit = {
    entityToProbe.remove(entity)
    entity.clearFlag(Flag_AmbientProbes)
  }

  private def updateVisibleProbe(result: ArrayBuffer[Probe], probe: ProbeImpl): Unit = {
    if (probe.visibleMark) return
    probe.visibleMark = true
    result += probe

    // Clear the probe's light
    probe.irradianceProbe.copyFrom(globalProbe)

    // Update probe position and indirect probes if it's dynamic
    if (!probe.static) {
      probe.worldPosition = probe.entity.position + probe.localPosition
      linkIndirectProbes(probe)
    }

    // Recursively mark indirect probes this probe depends on
    for (probe <- probe.indirectProbes) {
      updateVisibleProbe(result, probe.asInstanceOf[ProbeImpl])
    }
  }

  override def updateVisibleProbes(visible: EntitySet): ArrayBuffer[Probe] = {
    val result = new ArrayBuffer[Probe]()
    for (entity <- visible.flag(Flag_AmbientProbes)) {
      for (probe <- entityToProbe(entity)) {
        updateVisibleProbe(result, probe)
      }
    }
    result
  }

  override def updateIndirectLight(probes: ArrayBuffer[Probe]): Unit = {
    val up = Vector3.Up
    val down = Vector3.Down

    var ix = 0
    val numProbes = probes.length

    val falloff = 0.2
    val baseFactor = 0.3

    while (ix < numProbes) {
      val probe = probes(ix).asInstanceOf[ProbeImpl]
      if (probe.isIndirectEmitter) {
        val lightProbe = probe.irradianceProbe
        probe.cachedIndirectLight = lightProbe.evaluate(up)
        lightProbe.addDirectionalScaled(down, probe.cachedIndirectLight, baseFactor)
      }

      ix += 1
    }

    ix = 0
    while (ix < numProbes) {
      val probe = probes(ix).asInstanceOf[ProbeImpl]
      val lightProbe = probe.irradianceProbe
      val posY = probe.worldPosition.y

      if (probe.indirectProbes.nonEmpty) {
        var indIx = 0
        val indLen = probe.indirectProbes.length
        while (indIx < indLen) {
          val indProbe = probe.indirectProbes(indIx).asInstanceOf[ProbeImpl]
          val indWeight = probe.indirectWeights(indIx)

          val factor = baseFactor / (1.0 + math.abs(indProbe.worldPosition.y - posY) * falloff)

          lightProbe.addDirectionalScaled(down, indProbe.cachedIndirectLight, factor * indWeight)

          indIx += 1
        }
      }

      ix += 1
    }
  }

  override def frameCleanup(visibleProbes: ArrayBuffer[Probe]): Unit = {
    var ix = 0
    val len = visibleProbes.length
    while (ix < len) {
      visibleProbes(ix).asInstanceOf[ProbeImpl].visibleMark = false
      ix += 1
    }
  }

  override def entitiesDeleted(entities: EntitySet): Unit = entities.flag(Flag_AmbientProbes).foreach(removeProbes)

}
