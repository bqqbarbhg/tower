package ui

import asset.ShaderAsset
import core._
import render.VertexSpec.{Attrib, DataFmt}
import render._

import scala.collection.mutable.ArrayBuffer

object DebugDraw {

  object LineShader extends ShaderAsset("shader/debug_line") {
    uniform(VertexUniform)
    object VertexUniform extends UniformBlock("VertexUniform") {
      val ViewProjection = mat4("ViewProjection")
    }
  }

  val LineSpec = VertexSpec(Vector(
    Attrib(3, DataFmt.F32, Identifier("Position")),
    Attrib(4, DataFmt.UN8, Identifier("Color")),
  ))

  val MaxLinesPerFrame = 1024*8
  lazy val vertexBuffer = VertexBuffer.createDynamic(LineSpec, MaxLinesPerFrame * 4).withLabel("DebugDraw.vertexBuffer")

  var bufferOffset = 0

  case class Line(begin: Vector3, end: Vector3, beginColor: Color, endColor: Color)

  private val lines = new ArrayBuffer[Line]()

  def drawConnectedLine(vertices: Iterable[Vector3], color: Color): Unit = {
    drawConnectedLine(vertices.map(v => (v, color)))
  }

  def drawConnectedLine(vertices: Iterable[(Vector3, Color)]): Unit = {
    if (vertices.size < 2) return
    for ((prev, next) <- (vertices zip vertices.drop(1))) {
      val (p0, c0) = prev
      val (p1, c1) = next
      drawLine(p0, p1, c0, c1)
    }
  }

  def drawLine(begin: Vector3, end: Vector3, color: Color): Unit = drawLine(begin, end, color, color)
  def drawLine(begin: Vector3, end: Vector3, beginColor: Color, endColor: Color): Unit = drawLine(Line(begin, end, beginColor, endColor))
  def drawLine(line: Line): Unit = lines += line

  def render(viewProjection: Matrix4): Unit = {

    val numVerts = lines.length * 2
    if (bufferOffset + numVerts >= vertexBuffer.numVertices)
      bufferOffset = 0

    vertexBuffer.map(bufferOffset, numVerts, b => {

      for (line <- lines) {
        b.putFloat(line.begin.x.toFloat)
        b.putFloat(line.begin.y.toFloat)
        b.putFloat(line.begin.z.toFloat)
        b.putInt(line.beginColor.toSrgbInt32)

        b.putFloat(line.end.x.toFloat)
        b.putFloat(line.end.y.toFloat)
        b.putFloat(line.end.z.toFloat)
        b.putInt(line.endColor.toSrgbInt32)
      }

      numVerts
    })

    val renderer = Renderer.get

    LineShader.get.use()

    renderer.pushUniform(LineShader.VertexUniform, u => {
      LineShader.VertexUniform.ViewProjection.set(u, viewProjection)
    })

    renderer.drawLines(numVerts, vertexBuffer, baseVerterx = bufferOffset)

    bufferOffset += numVerts

    lines.clear()
  }

}

