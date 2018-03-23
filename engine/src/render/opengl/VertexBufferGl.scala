package render.opengl

import java.nio.ByteBuffer

import org.lwjgl.opengl.GL
import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl.GL30._
import org.lwjgl.opengl.GL44._
import core._
import org.lwjgl.system.MemoryUtil
import render._
import debug.ResourceHandle

object VertexBufferGl {
  var serialCounter = 0
  def nextSerial(): Int = {
    serialCounter += 1
    serialCounter
  }

  /** Create a vertex buffer with constant contents from `data` */
  def createStatic(spec: VertexSpec, data: ByteBuffer): VertexBufferGl = {
    val buf = new VertexBufferGl(spec, data.remaining / spec.sizeInBytes, false)
    glBindBuffer(GL_ARRAY_BUFFER, buf.buffer)
    glBufferData(GL_ARRAY_BUFFER, data, GL_STATIC_DRAW)
    glBindBuffer(GL_ARRAY_BUFFER, 0)
    buf
  }

  /** Create a dynamic vertex buffer that can hold `numVertices` vertices. */
  def createDynamic(spec: VertexSpec, numVertices: Int): VertexBufferGl = {
    new VertexBufferGl(spec, numVertices, true)
  }
}

class VertexBufferGl(val spec: VertexSpec, val numVertices: Int, val dynamic: Boolean) {
  val serial: Int = VertexBufferGl.nextSerial()
  val sizeInBytes = numVertices * spec.sizeInBytes

  private var mapMode = OptsGl.vertexMap
  private var persistentMap: ByteBuffer = null

  private var mapLoc = 0
  private var mapSize = 0
  private var mapBuf: ByteBuffer = null

  val buffer: Int = glGenBuffers()
  val debugRes = ResourceHandle("gl.vertexBuffer")

  // Initialize dynamic
  if (dynamic) {
    glBindBuffer(GL_ARRAY_BUFFER, buffer)

    if (mapMode.persistent) {
      if (GL.getCapabilities.GL_ARB_buffer_storage) {
        var flag = GL_MAP_WRITE_BIT | GL_MAP_PERSISTENT_BIT
        if (mapMode.coherent) flag |= GL_MAP_COHERENT_BIT
        var mapFlag = flag
        if (!mapMode.coherent) mapFlag |= GL_MAP_FLUSH_EXPLICIT_BIT
        glBufferStorage(GL_ARRAY_BUFFER, sizeInBytes, flag)
        persistentMap = glMapBufferRange(GL_ARRAY_BUFFER, 0, sizeInBytes, mapFlag)
      } else {
        mapMode = OptsGl.vertexMapFallback
      }
    }

    if (persistentMap == null)
      glBufferData(GL_ARRAY_BUFFER, sizeInBytes, GL_DYNAMIC_DRAW)

    glBindBuffer(GL_ARRAY_BUFFER, 0)
  }

  /**
    * Map a vertex buffer for writing and keep it mapped.
    * Call `endMap` to finish writing the data.
    *
    * @param begin First vertex to map
    * @param numVertices Number of vertices to map
    */
  def beginMap(begin: Int, numVertices: Int): ByteBuffer = {
    val loc = begin * spec.sizeInBytes
    val size = numVertices * spec.sizeInBytes
    mapLoc = loc
    mapSize = size

    glBindBuffer(GL_ARRAY_BUFFER, buffer)

    val buf = mapMode match {
      case MapMode.Map =>
        glMapBufferRange(GL_ARRAY_BUFFER, loc, size, GL_MAP_WRITE_BIT|GL_MAP_UNSYNCHRONIZED_BIT|GL_MAP_FLUSH_EXPLICIT_BIT)

      case MapMode.SubData =>
        MemoryUtil.memAlloc(size)

      case MapMode.Persistent|MapMode.PersistentCoherent =>
        persistentMap.sliced(loc, size)

      case MapMode.PersistentCopy|MapMode.PersistentCopyCoherent =>
        MemoryUtil.memAlloc(size)
    }

    mapBuf = buf
    glBindBuffer(GL_ARRAY_BUFFER, 0)
    buf
  }

  /**
    * Finish mapping the buffer.
    *
    * @param numVertices Number of vertices actually written.
    * @return
    */
  def endMap(numVertices: Int): Unit = {
    val loc = mapLoc
    val size = mapSize
    val buf = mapBuf

    glBindBuffer(GL_ARRAY_BUFFER, buffer)

    mapMode match {
      case MapMode.Map =>
        glFlushMappedBufferRange(GL_ARRAY_BUFFER, 0, numVertices * spec.sizeInBytes)
        glUnmapBuffer(GL_ARRAY_BUFFER)

      case MapMode.SubData =>
        buf.position(0)
        buf.limit(numVertices * spec.sizeInBytes)
        glBufferSubData(GL_ARRAY_BUFFER, loc, buf)
        MemoryUtil.memFree(buf)

      case MapMode.Persistent =>
        glFlushMappedBufferRange(GL_ARRAY_BUFFER, loc, numVertices * spec.sizeInBytes)

      case MapMode.PersistentCoherent => // Nop

      case MapMode.PersistentCopy|MapMode.PersistentCopyCoherent =>
        val toWrite = numVertices * spec.sizeInBytes
        val copy = persistentMap.slicedOffset(loc, toWrite)
        buf.position(0)
        buf.limit(toWrite)
        MemoryUtil.memCopy(buf, copy)
        if (mapMode.coherent)
          glFlushMappedBufferRange(GL_ARRAY_BUFFER, loc, toWrite)
        MemoryUtil.memFree(buf)
    }

    glBindBuffer(GL_ARRAY_BUFFER, 0)

    mapLoc = 0
    mapSize = 0
    mapBuf = null
  }

  /**
    * Maps a range of the vertex buffer for writing.
    *
    * @param begin First vertex to map
    * @param numVertices Number of vertices to map
    * @param write Function that takes the mapped range as an argument. The range
    *              is only valid for the duration of the `write` call. The function
    *              should return the number of vertices actually written.
    */
  def map(begin: Int, numVertices: Int, writeData: ByteBuffer => Int): Unit = withStack {

    val loc = begin * spec.sizeInBytes
    val size = numVertices * spec.sizeInBytes

    glBindBuffer(GL_ARRAY_BUFFER, buffer)

    mapMode match {
      case MapMode.Map =>
        val buf = glMapBufferRange(GL_ARRAY_BUFFER, loc, size, GL_MAP_WRITE_BIT|GL_MAP_UNSYNCHRONIZED_BIT|GL_MAP_FLUSH_EXPLICIT_BIT)
        val numVerts = writeData(buf)
        glFlushMappedBufferRange(GL_ARRAY_BUFFER, 0, numVerts * spec.sizeInBytes)
        glUnmapBuffer(GL_ARRAY_BUFFER)

      case MapMode.SubData =>
        val buf = alloca(size)
        val numVerts = writeData(buf)
        buf.position(0)
        buf.limit(numVerts * spec.sizeInBytes)
        glBufferSubData(GL_ARRAY_BUFFER, loc, buf)

      case MapMode.Persistent|MapMode.PersistentCoherent =>
        val buf = persistentMap.sliced(loc, size)
        val numVerts = writeData(buf)
        if (!mapMode.coherent)
          glFlushMappedBufferRange(GL_ARRAY_BUFFER, loc, numVerts * spec.sizeInBytes)

      case MapMode.PersistentCopy|MapMode.PersistentCopyCoherent =>
        val buf = alloca(size)
        val numVerts = writeData(buf)
        val toWrite = numVerts * spec.sizeInBytes
        val copy = persistentMap.slicedOffset(loc, toWrite)
        buf.position(0)
        buf.limit(toWrite)
        MemoryUtil.memCopy(buf, copy)
        if (!mapMode.coherent)
          glFlushMappedBufferRange(GL_ARRAY_BUFFER, loc, toWrite)
    }

    glBindBuffer(GL_ARRAY_BUFFER, 0)
  }

  def free(): Unit = {
    if (persistentMap != null) {
      glBindBuffer(GL_ARRAY_BUFFER, buffer)
      glUnmapBuffer(GL_ARRAY_BUFFER)
      glBindBuffer(GL_ARRAY_BUFFER, 0)
      persistentMap = null
    }

    glDeleteBuffers(buffer)
    debugRes.free()
  }

  def setLabel(label: String): Unit = DebugGl.setLabel(DebugGl.BUFFER, buffer, label)
  def withLabel(label: String): VertexBufferGl = {
    setLabel(label)
    this
  }
}

