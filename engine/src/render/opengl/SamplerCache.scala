package render.opengl

import scala.collection.mutable
import org.lwjgl.opengl.GL
import org.lwjgl.opengl.EXTTextureFilterAnisotropic._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL12._
import org.lwjgl.opengl.GL33._

import core._
import render._
import SamplerCache._

object SamplerCache {

  private case class Tag(sampler: Int)

  private class Entry {
    var tag: Tag = null
    var lastFrameUsed: Int = 0
    var sampler: Int = 0

    var prev: Entry = null
    var next: Entry = null
  }

}

/**
  * Cache that manages OpenGL Sampler Objects.
  */
class SamplerCache {

  val LruWaitFrames = 8

  private var currentFrame: Int = 0
  private val entries = new mutable.HashMap[Tag, Entry]()

  private var lruHead: Entry = null
  private var lruTail: Entry = null

  import Sampler.Wrap._
  import Sampler.Filter._

  private def toGlWrap(wrap: Sampler.Wrap): Int = wrap match {
    case Repeat => GL_REPEAT
    case Clamp  => GL_CLAMP_TO_EDGE
  }

  private def toGlMag(mag: Sampler.Filter): Int = mag match {
    case Nearest => GL_NEAREST
    case Linear  => GL_LINEAR
    case other => throw new AssertionError(s"Unsupported filter $other")
  }

  private def toGlMin(min: Sampler.Filter, mip: Sampler.Filter): Int = mip match {
    case Fixed   => min match {
      case Nearest => GL_NEAREST
      case Linear  => GL_LINEAR
      case other => throw new AssertionError(s"Unsupported filter $other")
    }
    case Nearest => min match {
      case Nearest => GL_NEAREST_MIPMAP_NEAREST
      case Linear  => GL_LINEAR_MIPMAP_NEAREST
      case other => throw new AssertionError(s"Unsupported filter $other")
    }
    case Linear  => min match {
      case Nearest => GL_NEAREST_MIPMAP_LINEAR
      case Linear  => GL_LINEAR_MIPMAP_LINEAR
      case other => throw new AssertionError(s"Unsupported filter $other")
    }
  }

  private val MaxAnisotropy = if (GL.getCapabilities.GL_EXT_texture_filter_anisotropic) {
    glGetFloat(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT)
  } else {
    0.0f
  }

  private def configureSampler(s: Int, sampler: Sampler): Unit = {
    glSamplerParameteri(s, GL_TEXTURE_WRAP_S, toGlWrap(sampler.wrapU))
    glSamplerParameteri(s, GL_TEXTURE_WRAP_T, toGlWrap(sampler.wrapV))
    glSamplerParameteri(s, GL_TEXTURE_MAG_FILTER, toGlMag(sampler.mag))
    glSamplerParameteri(s, GL_TEXTURE_MIN_FILTER, toGlMin(sampler.min, sampler.mip))

    if (MaxAnisotropy > 0.0f) {
      val aniso = clamp(sampler.maxAnisotropy.toFloat, 1.0f, MaxAnisotropy)
      glSamplerParameterf(s, GL_TEXTURE_MAX_ANISOTROPY_EXT, aniso)
    }

    DebugGl.setLabel(DebugGl.SAMPLER, s, sampler.debugName)
  }

  /** Needs to be called every frame */
  def advanceFrame(): Unit = {
    currentFrame += 1
  }

  /**
    * Retrieves a Sampler Object matching the description.
    */
  def getSampler(sampler: Sampler): Int = {
    val tag = Tag(sampler.serial)

    val entry = entries.getOrElseUpdate(tag, {

      // Try to unqueue the last element in the LRU cache
      val tail = lruTail
      val entry = if (tail != null && currentFrame >= tail.lastFrameUsed + LruWaitFrames) {
        // Success: Unlink, remove from hash-map, and return tail
        if (tail.prev != null) tail.prev.next = null
        lruTail = tail.prev
        entries -= tail.tag
        tail
      } else {
        // Fail: Create new entry
        val entry = new Entry()
        entry.sampler = glGenSamplers()
        entry
      }

      entry.tag = tag
      configureSampler(entry.sampler, sampler)

      entry
    })

    // Unlink from LRU list and insert to the head
    if (entry.prev != null) entry.prev.next = entry.next
    if (entry.next != null) entry.next.prev = entry.prev
    if (lruHead != null) lruHead.prev = entry
    entry.next = lruHead
    lruHead = entry
    if (lruTail == null) lruTail = entry

    entry.lastFrameUsed = currentFrame
    entry.sampler
  }

  /** Release used resources */
  def unload(): Unit = {
    for ((tag, entry) <- entries) {
      glDeleteSamplers(entry.sampler)
    }
    entries.clear()
    lruHead = null
    lruTail = null
  }
}


