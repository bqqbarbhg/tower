package ui

import asset._
import core._
import gfx.RingVertexBufferAsset
import render.VertexSpec.{Attrib, DataFmt}
import render._
import util.geometry.Aabb

import scala.collection.mutable.ArrayBuffer

object DebugDraw {

  object LineShader extends ShaderAsset("shader/debug/debug_line") {
    uniform(VertexUniform)
    object VertexUniform extends UniformBlock("VertexUniform") {
      val ViewProjection = mat4("ViewProjection")
    }
  }

  val LineSpec = VertexSpec(Vector(
    Attrib(3, DataFmt.F32, Identifier("Position")),
    Attrib(4, DataFmt.UN8, Identifier("Color")),
  ))

  val MaxLinesPerFrame = 1024*32

  val ringBuffer = new RingVertexBufferAsset("DebugDraw", LineSpec, MaxLinesPerFrame)

  var bufferOffset = 0

  case class Line(begin: Vector3, end: Vector3, beginColor: Color, endColor: Color)
  case class Line2D(begin: Vector2, end: Vector2, beginColor: Color, endColor: Color)

  private val lines = new ArrayBuffer[Line]()
  private val lines2D = new ArrayBuffer[Line2D]()

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

  def drawAabb(aabb: Aabb, color: Color): Unit = drawAabb(aabb.min, aabb.max, color)
  def drawAabb(min: Vector3, max: Vector3, color: Color): Unit = {
    drawLine(Vector3(min.x, min.y, min.z), Vector3(max.x, min.y, min.z), color)
    drawLine(Vector3(min.x, max.y, min.z), Vector3(max.x, max.y, min.z), color)
    drawLine(Vector3(min.x, min.y, max.z), Vector3(max.x, min.y, max.z), color)
    drawLine(Vector3(min.x, max.y, max.z), Vector3(max.x, max.y, max.z), color)

    drawLine(Vector3(min.x, min.y, min.z), Vector3(min.x, max.y, min.z), color)
    drawLine(Vector3(min.x, min.y, max.z), Vector3(min.x, max.y, max.z), color)
    drawLine(Vector3(min.x, min.y, min.z), Vector3(min.x, min.y, max.z), color)
    drawLine(Vector3(min.x, max.y, min.z), Vector3(min.x, max.y, max.z), color)

    drawLine(Vector3(max.x, min.y, min.z), Vector3(max.x, max.y, min.z), color)
    drawLine(Vector3(max.x, min.y, max.z), Vector3(max.x, max.y, max.z), color)
    drawLine(Vector3(max.x, min.y, min.z), Vector3(max.x, min.y, max.z), color)
    drawLine(Vector3(max.x, max.y, min.z), Vector3(max.x, max.y, max.z), color)
  }

  def drawLine2D(begin: Vector2, end: Vector2, color: Color): Unit = drawLine2D(begin, end, color, color)
  def drawLine2D(begin: Vector2, end: Vector2, beginColor: Color, endColor: Color): Unit = drawLine2D(Line2D(begin, end, beginColor, endColor))
  def drawLine2D(line: Line2D): Unit = lines2D += line

  def render(viewProjection: Matrix4): Unit = {
    if (lines.isEmpty) return

    val numVerts = lines.length * 2
    val rb = ringBuffer.get
    val bufferOffset = rb.push(numVerts)

    rb.buffer.map(bufferOffset, numVerts, b => {

      for (line <- lines) {
        b.putFloat(line.begin.x.toFloat)
        b.putFloat(line.begin.y.toFloat)
        b.putFloat(line.begin.z.toFloat)
        b.putInt(line.beginColor.toSrgbInt8)

        b.putFloat(line.end.x.toFloat)
        b.putFloat(line.end.y.toFloat)
        b.putFloat(line.end.z.toFloat)
        b.putInt(line.endColor.toSrgbInt8)
      }

      numVerts
    })

    val renderer = Renderer.get

    LineShader.get.use()

    renderer.pushUniform(LineShader.VertexUniform, u => {
      LineShader.VertexUniform.ViewProjection.set(u, viewProjection)
    })

    renderer.drawLines(numVerts, rb.buffer, baseVertex = bufferOffset)

    lines.clear()
  }

  def render2D(): Unit = {
    if (lines2D.isEmpty) return

    val numVerts = lines2D.length * 2
    val rb = ringBuffer.get
    val bufferOffset = rb.push(numVerts)

    rb.buffer.map(bufferOffset, numVerts, b => {

      for (line <- lines2D) {
        b.putFloat(line.begin.x.toFloat)
        b.putFloat(line.begin.y.toFloat)
        b.putFloat(0.5f)
        b.putInt(line.beginColor.toSrgbInt8)

        b.putFloat(line.end.x.toFloat)
        b.putFloat(line.end.y.toFloat)
        b.putFloat(0.5f)
        b.putInt(line.endColor.toSrgbInt8)
      }

      numVerts
    })

    val renderer = Renderer.get
    renderer.setDepth(Renderer.DepthNone)

    LineShader.get.use()

    val width = renderer.currentRenderTarget.width
    val height = renderer.currentRenderTarget.height
    val viewProjection = Matrix4.orthographicOffCenter(0.0, width, 0.0, height, 0.1, 1.0)

    renderer.pushUniform(LineShader.VertexUniform, u => {
      LineShader.VertexUniform.ViewProjection.set(u, viewProjection)
    })

    renderer.drawLines(numVerts, rb.buffer, baseVertex = bufferOffset)

    lines2D.clear()
  }

}

