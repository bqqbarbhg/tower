package asset
/*
import scala.reflect.ClassTag

import scala.collection.mutable.ArrayBuffer

import org.lwjgl.PointerBuffer
import org.lwjgl.assimp._
import org.lwjgl.assimp.Assimp._

import util.Serialization.ExtendedByteBuffer

object Quaternion {

  def lerp(a: Quaternion, b: Quaternion, t: Float): Quaternion = a * (1.0f - t) + b * t

}

case class Quaternion(x: Float, y: Float, z: Float, w: Float) {

  def length: Float = math.sqrt(x*x + y*y + z*z + w*w).toFloat
  def normalize: Quaternion = {
    val il = 1.0f / this.length
    Quaternion(x * il, y * il, z * il, w * il)
  }

  def *(f: Float): Quaternion = Quaternion(x * f, y * f, z * f, w * f)
  def /(f: Float): Quaternion = this * (1.0f / f)
  def +(q: Quaternion): Quaternion = Quaternion(x + q.x, y + q.y, z + q.z, w + q.w)
  def -(q: Quaternion): Quaternion = Quaternion(x - q.x, y - q.y, z - q.z, w - q.w)

}

case class FrameRot(time: Double, rot: Quaternion)

class FbxAsset(filename: String) {

  def collect[T: ClassTag](buf: PointerBuffer, num: Int, ctor: Long => T): Array[T] = {
    val iter = for (i <- 0 until num) yield ctor(buf.get(i))
    iter.toArray
  }

  def optimizeRotationKeyframes(frames: Seq[FrameRot], maxError: Float): ArrayBuffer[FrameRot] = {
    val result = new ArrayBuffer[FrameRot]()
    if (frames.length == 0) return result
    result += frames.head
    if (frames.length == 1) return result

    var base = 0
    var top = 1
    while (top < frames.length) {

      val a = frames(base).rot
      val b = frames(top).rot
      val start = frames(base).time
      val duration = frames(top).time - start

      val good = frames.slice(base + 1, top).forall(frame => {
        val t = (frame.time - start) / duration
        val ref = Quaternion.lerp(a, b, t.toFloat).normalize
        val error = frame.rot - ref
        error.length <= maxError
      })

      if (good) {
        top += 1
      } else {
        result += frames(top - 1)
        base = top - 1
        top = base + 2
      }
    }

    result += frames.last

    result
  }

  def process(): Unit = {

    val scene = aiImportFile(filename, aiProcess_Triangulate | aiProcess_JoinIdenticalVertices)
    val animations = collect(scene.mAnimations, scene.mNumAnimations, AIAnimation.create)

    for (animation <- animations) {
      val name = animation.mName.dataString
      val channels = collect(animation.mChannels, animation.mNumChannels, AINodeAnim.create)

      for (channel <- channels) {
        println(s"Channel: ${channel.mNodeName.dataString}")
        println(s"  Rotation keys: ${channel.mNumRotationKeys}")

        val frames = for (rotI <- 0 until channel.mNumRotationKeys) yield {
          val rot = channel.mRotationKeys.get(rotI)
          val qt = rot.mValue
          FrameRot(rot.mTime, Quaternion(qt.x, qt.y, qt.z, qt.w))
        }

        val error = 0.01f
        val newFrames = optimizeRotationKeyframes(frames, error)
        println(s"  Optimized rotation keys: ${newFrames.length} (max error: ${error * 100.0f}%)")
      }

      val Version = 1

      val buffer = java.nio.ByteBuffer.allocateDirect(1024 * 1024)
      buffer.putMagic("S2AN")
      buffer.putInt(Version)
      buffer.putString(name, stringPool)


    }

    aiReleaseImport(scene)
  }

}
*/
