package game.system

import core._
import game.lighting.LightProbe

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object LightSystem {

  class ProbeRef {
    var probe: LightProbe = LightProbe.make()

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
      val rounded = position /@ cellSize
      val x = math.floor(rounded.x).toInt
      val y = math.floor(rounded.x).toInt
      val z = math.floor(rounded.x).toInt
      CellPos(x, y, z)
    }

    def getLights(position: Vector3): Iterable[InternalLightRef] = {
      map.get(getCellPos(position)).toIterable.flatMap(_.lights)
    }
  }

  val globalProbe = LightProbe.make()

  private val smallGrid = new Grid(Vector3(20.0, 40.0, 20.0))
  private val bigGrid = new Grid(Vector3(80.0, 400.0, 80.0))

  private def findBestGrid(radius: Double): Grid = {
    if (radius <= 40.0)
      smallGrid
    else
      bigGrid
  }

  private val probes = new ArrayBuffer[InternalProbeRef]()
  private val dynamicLights = new ArrayBuffer[InternalLightRef]()
  private val cellsWithDynamicLights = new ArrayBuffer[Cell]()

  private val groundProbes = new ArrayBuffer[InternalProbeRef]()
  private val nonGroundProbes = new ArrayBuffer[InternalProbeRef]()

  def addStaticProbe(position: Vector3): ProbeRef = {
    val probe = new InternalProbeRef()
    probe.position = position
    probes += probe
    nonGroundProbes += probe
    probe
  }

  def addStaticGroundProbe(position: Vector3): ProbeRef = {
    val probe = new InternalProbeRef()
    probe.position = position
    probes += probe
    groundProbes += probe
    probe
  }

  def addDynamicProbe(position: Vector3): DynamicProbeRef = addDynamicProbe(null, position)

  def addDynamicProbe(parent: Entity, position: Vector3): DynamicProbeRef = {
    val probe = new InternalProbeRef()
    probe.position = position
    probe.parent = parent
    probes += probe
    nonGroundProbes += probe
    probe
  }

  private def insertLight(pos: Vector3, light: InternalLightRef, static: Boolean): Unit = {
    val radius = light.radius
    val grid = findBestGrid(radius)

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

    dynamicLights += light

    light
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

      if (length < light.radius) {
        val falloffDist = 1.0 - length / light.radius
        val falloff = falloffDist * falloffDist

        var ratio = 1.0
        if (length <= 1.0) {
          ratio = math.max((length - 0.5) * 2.0, 0.0)
          probe.addGlobal(light.intensity * (1.0 - ratio) * falloff)
        }
        if (length >= 0.5) {
          val falloff = falloffDist * falloffDist
          val normal = dir / length
          probe.addDirectional(normal, light.intensity * ratio * falloff)
        }
      }

    }
  }

  def evaluateProbes(): Unit = {
    for (probeRef <- probes) {
      probeRef.probe.copyFrom(globalProbe)
      addLightsToProbe(probeRef, smallGrid.getLights(probeRef.position))
      addLightsToProbe(probeRef, bigGrid.getLights(probeRef.position))
    }

    var groundProbe = new Array[LightProbe](4)
    var groundWeight = new Array[Double](4)
    var temp = LightProbe.make()
    val up =   Vector3(0.0, +1.0, 0.0)
    val down = Vector3(0.0, -1.0, 0.0)

    for (probe <- nonGroundProbes) {
      GroundSystem.getProbesAndWeights(probe.position, groundProbe, groundWeight)

      temp.clear()
      temp.addScaled(groundProbe(0), groundWeight(0))
      temp.addScaled(groundProbe(1), groundWeight(1))
      temp.addScaled(groundProbe(2), groundWeight(2))
      temp.addScaled(groundProbe(3), groundWeight(3))

      val intensity = temp.evaluate(up)
      probe.probe.addDirectional(down, intensity * 0.35)
    }

    for (probe <- groundProbes) {
      val intensity = probe.probe.evaluate(up)
      probe.probe.addDirectional(down, intensity * 0.35)
    }

  }

  def finishFrame(): Unit = {
    for (cell <- cellsWithDynamicLights) {
      cell.lights.remove(cell.numStatic, cell.lights.length - cell.numStatic)
    }
    cellsWithDynamicLights.clear()
  }

}
