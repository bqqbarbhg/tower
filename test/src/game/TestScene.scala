package game

import java.io.File

import util.BufferUtils._
import java.nio.{ByteBuffer, ByteOrder}
import javax.management.openmbean.CompositeData
import javax.management.{Notification, NotificationEmitter, NotificationListener}

import com.sun.management.GarbageCollectionNotificationInfo

import collection.JavaConverters._
import core._
import render._
import render.UniformBlock
import platform.AppWindow

import scala.io.StdIn

object TestScene extends App {

  core.StackAllocator.createCurrentThread(16 * 1024 * 1024)

  object ModelTextures extends SamplerBlock {
    val Diffuse = sampler2D("Diffuse", Sampler.RepeatAnisotropic)
  }

  object ModelUniform extends UniformBlock("ModelUniform") {
    val ViewProjection = mat4("ViewProjection")
    val World = mat4x3("World")
  }

  val VertexShader =
    """
#version 150

in vec3 a_Position;
in vec2 a_TexCoord;

layout(row_major)
layout(std140)
uniform ModelUniform {
  mat4 u_ViewProjection;
  mat4x3 u_World;
};

out vec2 v_TexCoord;

void main() {
  vec3 world = u_World * vec4(a_Position, 1.0);
  gl_Position = u_ViewProjection * vec4(world, 1.0);
  v_TexCoord = a_TexCoord;
}
    """

  val FragmentShader =
    """
#version 150

uniform sampler2D u_Diffuse;

in vec2 v_TexCoord;
out vec4 o_Color;

void main() {
  o_Color = texture(u_Diffuse, v_TexCoord);
}
    """


  val arg = util.ArgumentParser.parse(args)

  if (arg.flag("listen-gc")) {
    object GcListener extends NotificationListener {
      override def handleNotification(notification: Notification, handback: Any): Unit = {
        if (notification.getType == GarbageCollectionNotificationInfo.GARBAGE_COLLECTION_NOTIFICATION) {
          val info = GarbageCollectionNotificationInfo.from(notification.getUserData.asInstanceOf[CompositeData])
          println(s"GC [${info.getGcName}]: ${info.getGcAction}, cause: ${info.getGcCause}. Took ${info.getGcInfo.getDuration}ms")
        }
      }
    }

    val gcBeans = java.lang.management.ManagementFactory.getGarbageCollectorMXBeans().asScala
    for (gcBean <- gcBeans) {
      val emitter = gcBean.asInstanceOf[NotificationEmitter]
      emitter.addNotificationListener(GcListener, null, null)
    }
  }

  val debug = arg.flag("debug")
  AppWindow.initialize(1280, 720, "Test window", debug)

  val renderer = Renderer.initialize()

  val shader = Shader.compile(VertexShader, FragmentShader, ModelTextures, ModelUniform)

  val viewProjection = (
      Matrix4.perspective(1280.0 / 720.0, math.Pi / 3.0, 0.01, 1000.0)
      * Matrix43.look(Vector3(0.0, 0.0, -10.0), Vector3(0.0, 0.0, 1.0)))

  val buffer = ByteBuffer.allocateDirect(16 * 1024 * 1024)

  val mesh = {
    val file = new File("data/test/sausageman.fbx.Cube.000.s2ms")
    var buf = buffer.duplicateEx
    buf.order(ByteOrder.LITTLE_ENDIAN)
    buf.readFromFile(file)
    buf.finish()

    val mesh = new gfx.Mesh()
    mesh.load(buf)
    mesh
  }

  val texture = {
    val file = new File("data/test/grass.jpg.s2tx")
    var buf = buffer.duplicateEx
    buf.order(ByteOrder.LITTLE_ENDIAN)
    buf.readFromFile(file)
    buf.finish()

    val tex = new gfx.Texture()
    tex.load(buf)
    tex
  }

  var prevNs = java.lang.System.nanoTime()
  var sampledTimesMs = Array.fill(60/**100*/)(0.0)
  var ix = 0

  renderer.setDepthMode(true, true)

  System.gc()

  var time = 0.0
  while (AppWindow.running) {
    val begin = java.lang.System.nanoTime()

    AppWindow.pollEvents()

    time = AppWindow.currentTime
    val world = Matrix43.translate(0.0, -5.0, 0.0) * Matrix43.rotateY(time) * Matrix43.rotateX(math.Pi / 2.0)

    renderer.advanceFrame()

    renderer.clear(Some(Color.rgb(0x6495ED)), Some(1.0))

    renderer.setShader(shader)

    renderer.pushUniform(ModelUniform, u => {
      import ModelUniform._
      ViewProjection.set(u, viewProjection)
      World.set(u, world)
    })


    renderer.setTexture(ModelTextures.Diffuse, texture.texture)

    for (part <- mesh.parts) {
      part.draw()
    }

    AppWindow.swapBuffers()

    val end = java.lang.System.nanoTime()

    {
      var diff = end - begin
      val diffMs = diff.toDouble / 1000.0 / 1000.0
      sampledTimesMs(ix) = diffMs

      ix += 1
      if (ix == sampledTimesMs.length) {
        ix = 0
        val avg = sampledTimesMs.sum / sampledTimesMs.length
        val min = sampledTimesMs.reduce((a, b) => math.min(a, b))
        val max = sampledTimesMs.reduce((a, b) => math.max(a, b))
        println(f"Frame time: $min%.2fms - $max%.2fms, average $avg%.2fms")
      }
    }
  }

  AppWindow.unload()
}
