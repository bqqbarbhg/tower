package game.system.rendering

import scala.collection.mutable
import core._
import game.system._
import game.system.Entity._
import game.lighting.LightProbe
import game.system.rendering.AmbientSystem.Probe
import game.system.rendering.AmbientPointLightSystem._
import game.system.rendering.AmbientPointLightSystemImpl._

import scala.collection.mutable.ArrayBuffer

object AmbientPointLightSystem {

  sealed abstract class AmbientPointLight(val entity: Entity) {
    var localPosition: Vector3 = Vector3.Zero
    var worldPosition: Vector3 = Vector3.Zero
    var intensity: Vector3 = Vector3.Zero
    var radius: Double = 0.0
  }

}

sealed trait AmbientPointLightSystem extends EntityDeleteListener {

  /**
    * Add a point light attached to `entity`.
    */
  def addLight(entity: Entity, localPosition: Vector3, intensity: Vector3, radius: Double): AmbientPointLight

  /**
    * Remove point-light containing `entity` from the system.
    */
  def removeLights(entity: Entity): Unit

  /**
    * Add point-light to visible ambient probes.
    */
  def updateVisibleProbes(probes: ArrayBuffer[Probe]): Unit

  /**
    * Insert dynamic lights into the spatial partitioning strucure.
    */
  def updateDynamicLights(): Unit

  /**
    * Clean-up to do after all the processing is done.
    */
  def frameCleanup(): Unit

}

object AmbientPointLightSystemImpl {

  final class AmbientPointLightImpl(entity: Entity, val next: AmbientPointLightImpl) extends AmbientPointLight(entity) {
    val static: Boolean = entity.static
    var dynamicIndex: Int = -1
    var grid: Grid = null
    var min: CellPos = null
    var max: CellPos = null
  }

  case class CellPos(x: Int, z: Int)

  val NoLights = ArrayBuffer[AmbientPointLightImpl]()

  class Cell {
    val lights = new ArrayBuffer[AmbientPointLightImpl]()
    var numStatic: Int = 0
  }

  class Grid(val cellSize: Vector3) {
    val map = new mutable.HashMap[CellPos, Cell]()

    def getCellPos(position: Vector3): CellPos = {
      val x = math.floor(position.x / cellSize.x).toInt
      val z = math.floor(position.z / cellSize.z).toInt
      CellPos(x, z)
    }

    def getLights(position: Vector3): ArrayBuffer[AmbientPointLightImpl] = {
      val cell = map.getOrElse(getCellPos(position), null)
      if (cell != null) {
        cell.lights
      } else {
        NoLights
      }
    }
  }

}

final class AmbientPointLightSystemImpl extends AmbientPointLightSystem {
  val entityToLight = new mutable.HashMap[Entity, AmbientPointLightImpl]()
  val dynamicLights = new ArrayPool[AmbientPointLightImpl]()
  val cellsWithDynamicLights = new ArrayBuffer[Cell]()

  val smallGrid = new Grid(Vector3(40.0, 40.0, 40.0))
  val bigGrid = new Grid(Vector3(80.0, 400.0, 80.0))

  def findBestGrid(radius: Double): Grid = {
    if (radius <= 40.0)
      smallGrid
    else
      bigGrid
  }

  def updateLight(light: AmbientPointLightImpl): Unit = {
    light.worldPosition = light.entity.position + light.localPosition

    val pos = light.worldPosition
    val radius = light.radius
    val grid = findBestGrid(radius)

    val radius3 = Vector3(radius, radius, radius)
    val min = grid.getCellPos(pos - radius3)
    val max = grid.getCellPos(pos + radius3)

    light.grid = grid
    light.min = min
    light.max = max

    for {
      x <- min.x to max.x
      z <- min.z to max.z
    } {
      val pos = CellPos(x, z)
      val cell = grid.map.getOrElseUpdate(pos, new Cell())
      if (light.static)
        cell.numStatic += 1
      else if (cell.numStatic == cell.lights.length) {
        // If this is the first dynamic light on this cell on the current update
        // add this cell to the list of dynamic cells.
        cellsWithDynamicLights += cell
      }

      cell.lights += light
    }
  }

  def removeStaticLight(light: AmbientPointLightImpl): Unit = {
    if (light.grid == null) return

    for {
      x <- light.min.x to light.max.x
      z <- light.min.z to light.max.z
    } {
      val pos = CellPos(x, z)
      for (cell <- light.grid.map.get(pos)) {
        cell.lights -= light
        cell.numStatic -= 1
      }
    }
  }

  def addLightToProbe(probe: Probe, light: AmbientPointLightImpl): Unit = {
    val dir = (light.worldPosition - probe.worldPosition)
    val length = dir.length

    if (length < light.radius) {
      val falloffDist = 1.0 - length / light.radius
      val falloff = falloffDist * falloffDist

      var ratio = 1.0
      if (length <= 0.5) {
        ratio = math.max((length - 0.5) * 2.0, 0.0)
        probe.irradianceProbe.addGlobal(light.intensity * (1.0 - ratio) * falloff)
      }
      if (length >= 0.5) {
        val falloff = falloffDist * falloffDist
        val normal = dir / length
        probe.irradianceProbe.addDirectionalScaled(normal, light.intensity, ratio * falloff)
      }
    }
  }

  def addLightsToProbe(probe: Probe, lights: ArrayBuffer[AmbientPointLightImpl]): Unit = {
    val lightProbe = probe.irradianceProbe
    var ix = 0
    val len = lights.length
    while (ix < len) {
      addLightToProbe(probe, lights(ix))
      ix += 1
    }
  }

  override def addLight(entity: Entity, localPosition: Vector3, intensity: Vector3, radius: Double): AmbientPointLight = {
    val next = if (entity.hasFlag(Flag_AmbientPointLight)) {
      entityToLight(entity)
    } else null

    val light = new AmbientPointLightImpl(entity, next)
    light.localPosition = localPosition
    light.intensity = intensity
    light.radius = radius

    if (entity.static) {
      updateLight(light)
    } else {
      light.dynamicIndex = dynamicLights.add(light)
    }

    entity.setFlag(Flag_AmbientPointLight)

    entityToLight(entity) = light

    light
  }

  override def removeLights(entity: Entity): Unit = {
    var light = entityToLight(entity)
    while (light != null) {

      if (light.static)
        removeStaticLight(light)

      if (light.dynamicIndex >= 0)
        dynamicLights.remove(light.dynamicIndex)

      light = light.next
    }
    entityToLight.remove(entity)

    entity.clearFlag(Flag_AmbientPointLight)
  }

  override def updateDynamicLights(): Unit = {
    var ix = 0
    val len = dynamicLights.sparseData.length
    while (ix < len) {
      val light = dynamicLights.sparseData(ix)
      if (light != null) {
        updateLight(light)
      }
      ix += 1
    }
  }

  override def updateVisibleProbes(probes: ArrayBuffer[Probe]): Unit = {
    var ix = 0
    val len = probes.length
    while (ix < len) {
      val probe = probes(ix)

      addLightsToProbe(probe, smallGrid.getLights(probe.worldPosition))
      addLightsToProbe(probe, bigGrid.getLights(probe.worldPosition))

      ix += 1
    }
  }

  override def frameCleanup(): Unit = {
    for (cell <- cellsWithDynamicLights) {
      cell.lights.remove(cell.numStatic, cell.lights.length - cell.numStatic)
    }
    cellsWithDynamicLights.clear()
  }

  override def entitiesDeleted(entities: EntitySet): Unit = entities.flag(Flag_AmbientPointLight).foreach(removeLights)
}

