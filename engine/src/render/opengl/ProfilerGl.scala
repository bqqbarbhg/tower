package render.opengl

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl.GL33._

import scala.collection.mutable

object ProfilerGl {

  class GpuTimestamp(private[opengl] val query: Int) {
    var ready: Boolean = false
    var timeNs: Long = 0
  }

  class GpuTimeSpan {
    private[opengl] var begin: GpuTimestamp = null
    private[opengl] var end: GpuTimestamp = null

    def stop(): Unit = {
      end = ProfilerGl.getGpuTimestamp()
    }

    def ready: Boolean = begin.ready && end.ready
    def durationNs: Long = end.timeNs - begin.timeNs
  }

  private val freeQueries = mutable.ArrayStack[Int]()
  private val pendingTimestamps = mutable.Queue[GpuTimestamp]()

  private def allocateQuery(): Int = {
    if (freeQueries.nonEmpty) {
      freeQueries.pop()
    } else {
      glGenQueries()
    }
  }

  private def freeQuery(name: Int): Unit = {
    freeQueries.push(name)
  }

  def startSpan(): GpuTimeSpan = {
    val span = new GpuTimeSpan()
    span.begin = getGpuTimestamp()
    span
  }

  def advanceFrame(): Unit = {
    while (pendingTimestamps.nonEmpty) {
      val ts = pendingTimestamps.front
      val status = glGetQueryObjecti(ts.query, GL_QUERY_RESULT_AVAILABLE)
      if (status != GL_TRUE) return
      ts.timeNs = glGetQueryObjecti64(ts.query, GL_QUERY_RESULT)
      ts.ready = true
      freeQuery(ts.query)
      pendingTimestamps.dequeue()
    }
  }

  def getGpuTimestamp(): GpuTimestamp = {
    val query = allocateQuery()
    val ts = new GpuTimestamp(query)
    glQueryCounter(query, GL_TIMESTAMP)
    pendingTimestamps += ts
    ts
  }

}

