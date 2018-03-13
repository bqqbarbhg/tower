package game.system

import core._
import game.gfx.LightProbe

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object LightSystem {

  class ProbeRef {
    var probe: LightProbe = null

    def delete(): Unit = {
    }
  }

  class DynamicProbeRef extends ProbeRef {
    var position: Vector3 = Vector3.Zero
  }

  private class InternalProbeRef extends DynamicProbeRef {
    var parent: Entity = null
  }

  class LightRef {
    def delete(): Unit = {
    }
  }

  class DynamicLightRef extends LightRef {
    var position: Vector3 = Vector3.Zero
    var intensity: Vector3 = Vector3.Zero
    var radius: Double = 0.0
  }

  private class InternalLightRef extends DynamicLightRef {
    var parent: Entity = null
  }

  case class CellPos(x: Int, y: Int, z: Int)

  private class Cell {
    val lights = new ArrayBuffer[InternalLightRef]()
    var numStatic: Int = 0
  }

  private class Grid(val cellSize: Vector3) {
    val map = new mutable.HashMap[CellPos, Cell]()

    def getCellPos(position: Vector3): CellPos = {
      val rounded = position / cellSize
      val x = math.floor(rounded.x).toInt
      val y = math.floor(rounded.x).toInt
      val z = math.floor(rounded.x).toInt
      CellPos(x, y, z)
    }

    def getLights(position: Vector3): Iterable[InternalLightRef] = {
      map.get(getCellPos(position)).toIterable.flatten
    }
  }

  val smallGrid = new Grid(Vector3(10.0, 20.0, 10.0))
  val bigGrid = new Grid(Vector3(40.0, 60.0, 40.0))

  def findBestGrid(radius: Double): Grid = {
    if (radius <= 40.0)
      smallGrid
    else
      bigGrid
  }

  val probes = new ArrayBuffer[InternalProbeRef]()
  val dynamicLights = new ArrayBuffer[InternalLightRef]()
  val cellsWithDynamicLights = new ArrayBuffer[Cell]()

  def addStaticProbe(position: Vector3): ProbeRef = {
    val probe = new InternalProbeRef()
    probe.position = position
    probes += probe
  }

  def addDynamicProbe(position: Vector3): DynamicProbeRef = addDynamicProbe(null, position)

  def addDynamicProbe(parent: Entity, position: Vector3): DynamicProbeRef = {
    val probe = new InternalProbeRef()
    probe.position = position
    probe.parent = parent
    probes += probe
  }

  private def insertLight(pos: Vector3, light: InternalLightRef, static: Boolean): Unit = {
    val grid = findBestGrid(radius)

    val radius = light.radius
    val radius3 = Vector3(radius, radius, radius)
    val min = grid.getCellPos(pos - radius3)
    val max = grid.getCellPos(pos + radius3)

    for {
      x <- min.x to max.x
      y <- min.y to max.y
      z <- min.z to max.z
    } {
      val pos = CellPos(x, y, z)
      val cell = grid.map.getOrElseUpdate(pos, new Cell())
      if (static)
        cell.numStatic += 1
      else if (cell.numStatic == cell.lights.length)
        cellsWithDynamicLights += cell

      cell.lights += light
    }
  }

  def addStaticLight(position: Vector3, intensity: Vector3, radius: Double): LightRef = {

    val light = new InternalLightRef()
    light.position = position
    light.intensity = intensity
    light.radius = radius

    insertLight(position, light, true)

    light
  }

  def addDynamicLight(position: Vector3, intensity: Vector3, radius: Double): DynamicLightRef =
    addDynamicLight(null, position, intensity, radius)

  def addDynamicLight(parent: Entity, position: Vector3, intensity: Vector3, radius: Double): DynamicLightRef = {

    val light = new InternalLightRef()
    light.parent = parent
    light.position = position
    light.intensity = intensity
    light.radius = radius

  }

  def addDynamicLightsToCells(): Unit = {
    for (light <- dynamicLights) {
      if (light.parent != null)
        insertLight(light.parent.position + light.position, light, false)
      else
        insertLight(light.position, light, false)
    }
  }

  def addLightsToProbe(probeRef: InternalProbeRef, lights: Iterable[InternalLightRef]): Unit = {
    val pos = probeRef.position
    val probe = probeRef.probe

    for (light <- lights) {
      val dir = (light.position - pos)
      val length = dir.length

      var ratio = 1.0
      if (length <= 1.0) {
        ratio = math.max((length - 0.5) * 2.0, 0.0)
        probe.addGlobal(light.intensity * (1.0 - ratio))
      }
      if (length >= 0.5) {
        val falloffDist = (0.5 + length)
        val falloff = falloffDist * falloffDist
        val normal = dir / length
        probe.addDirectional(normal, light.intensity * ratio / falloff)
      }

    }
  }

  def evaluateProbes(): Unit = {
    for (probeRef <- probes) {
      probeRef.copyFrom(globalProbe)
      addLightsToProbe(probeRef, smallGrid.getLights(probeRef.position))
      addLightsToProbe(probeRef, bigGrid.getLights(probeRef.position))
    }
  }

  def finishFrame(): Unit = {
    for (cell <- cellsWithDynamicLights) {
      cell.lights.remove(cell.numStatic, cell.lights.length - cell.numStatic)
    }
    cellsWithDynamicLights.clear()
  }

}
