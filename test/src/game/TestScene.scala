package game

import java.io.File

import util.BufferUtils._
import java.nio.{ByteBuffer, ByteOrder}
import javax.management.openmbean.CompositeData
import javax.management.{Notification, NotificationEmitter, NotificationListener}

import com.sun.management.GarbageCollectionNotificationInfo

import collection.JavaConverters._
import core._
import gfx.Shader.NoPermutations
import gfx._
import render._
import render.UniformBlock
import platform.AppWindow
import ui.{Atlas, Font, Sprite, SpriteBatch}
import io.content._
import render.opengl._
import ui.Font.TextDraw

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

object TestScene extends App {

  println("Version 1.1")

  core.StackAllocator.createCurrentThread(16 * 1024 * 1024)

  val pack = new MultiPackage()

  JarPackage.create("data") match {
    case Some(jar) => pack.add(jar, 1)
    case None => // Nop
  }

  pack.add(new DirectoryPackage("data"), 0)

  Package.set(pack)

  object ModelTextures extends SamplerBlock {
    val Diffuse = sampler2D("Diffuse", Sampler.RepeatAnisotropic)
    val Normal = sampler2D("Normal", Sampler.RepeatAnisotropic)
  }

  object ModelUniform extends UniformBlock("ModelUniform") {
    val ViewProjection = mat4("ViewProjection")
    val Bones = mat4x3("Bones", 24)
  }

  val arg = util.ArgumentParser.parse(args)

  if (arg.flag("gl-no-ubo")) OptsGl.useUniformBlocks = false
  if (arg.flag("gl-no-vao")) OptsGl.useVaoCache = false

  if (arg.flag("gl-compat")) {
    OptsGl.uniformMap = MapMode.SubData
    OptsGl.vertexMap = MapMode.SubData
    OptsGl.useTexStorage = false
    OptsGl.useUniformBlocks = false
    OptsGl.useVaoCache = false
  }

  if (arg.flag("debug")) {
    def printInfo(name: String, value: String): Unit = {
      println(f"$name%-20s = $value")
    }

    println("GL configuration:")
    printInfo("vertexMap", OptsGl.vertexMap.toString)
    printInfo("vertexMapFallback", OptsGl.vertexMapFallback.toString)
    printInfo("uniformMap", OptsGl.uniformMap.toString)
    printInfo("uniformMapFallback", OptsGl.uniformMapFallback.toString)
    printInfo("useTexStorage", OptsGl.useTexStorage.toString)
    printInfo("useUniformBlocks", OptsGl.useUniformBlocks.toString)
    printInfo("useVaoCache", OptsGl.useVaoCache.toString)
  }

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

  val shader = Shader.load("test/test_mesh", NoPermutations, ModelTextures, ModelUniform)

  val viewProjection = (
      Matrix4.perspective(1280.0 / 720.0, math.Pi / 3.0, 0.01, 1000.0)
      * Matrix43.look(Vector3(0.0, 0.0, -10.0), Vector3(0.0, 0.0, 1.0)))

  val buffer = ByteBuffer.allocateDirect(16 * 1024 * 1024)

  val model = gfx.Model.load(Identifier("test/sausageman/sausagemanWithTex.fbx.s2md")).get
  model.loadContent()

  val anim = model.anims.head

  val atlases = Package.get.list("atlas").filter(_.name.endsWith(".s2at"))
  for (atlas <- atlases) {
    println(s"Loading atlas: ${atlas.name}")
    Atlas.load(Identifier(atlas.name))
  }

  val sprite = Sprite.SpriteMap.get(Identifier("sprites/a.png"))
  Sprite.SpriteMap.atlases(sprite.atlas).loadTextures()

  val sb = new SpriteBatch()

  val font = Font.load("font/open-sans/OpenSans-Regular.ttf.s2ft").get

  var prevNs = java.lang.System.nanoTime()
  var sampledTimesMs = Array.fill(60/**100*/)(0.0)
  var ix = 0

  System.gc()

  val modelState = new ModelState(model)
  val animState = new AnimationState(model, anim)

  var time = 0.0
  while (AppWindow.running) {
    val begin = java.lang.System.nanoTime()

    AppWindow.pollEvents()

    time = AppWindow.currentTime
    val world = Matrix43.translate(0.0, -4.0, 0.0) * Matrix43.rotateY(time * 0.5) * Matrix43.scale(0.01)

    renderer.advanceFrame()

    renderer.setDepthMode(true, true)
    renderer.clear(Some(Color.rgb(0x6495ED)), Some(1.0))

    shader.use()

    renderer.setDepthMode(true, true)
    renderer.setBlend(false)

    modelState.worldTransform = world
    animState.time = time % anim.duration
    animState.alpha = math.sin(time) * 0.5 + 0.5

    animState.apply(modelState)

    modelState.updateMatrices()

    for (mesh <- model.meshes) {
      renderer.setTexture(ModelTextures.Diffuse, mesh.material.albedoTex.texture)
      renderer.setTexture(ModelTextures.Normal, mesh.material.normalTex.texture)

      for (part <- mesh.parts) {

        renderer.pushUniform(ModelUniform, u => {
          import ModelUniform._
          ViewProjection.set(u, viewProjection)

          for ((name, index) <- part.boneName.zipWithIndex) {
            val node = model.findNodeByName(new Identifier(name))
            val transform = modelState.nodeWorldTransform(node) * part.boneMeshToBone(index)
            Bones.set(u, index, transform)
          }

        })

        part.draw()
      }
    }

    renderer.setDepthMode(false, false)
    renderer.setBlend(true)

    val fg = Color.rgb(0xffffff)
    val bg = Color.rgb(0x000000)

    val draws = ArrayBuffer[TextDraw]()
    draws += TextDraw("This is a long body text that", 0, "This is a long body text that".length, Vector2(100.0, 114.0), 22.0, fg, 0.0, 1)
    draws += TextDraw("Hello world!", 0, "Hello world!".length, Vector2(100.0, 90.0), 82.0, fg, 0.0, 1)
    draws += TextDraw("spans many lines but is currently", 0, "spans many lines but is currently".length, Vector2(100.0, 128.0), 22.0, fg, 0.0, 1)
    draws += TextDraw("Hello world!", 0, "Hello world!".length, Vector2(100.0, 90.0), 82.0, bg, 2.0, 0)
    draws += TextDraw("manually wrapped...", 0, "manually wrapped...".length, Vector2(100.0, 142.0), 22.0, fg, 0.0, 1)

    font.render(draws)

    val names = ('a' to 'z').map(_.toString) ++ Seq("face")
    for ((sprite, i) <- names.zipWithIndex) {
      val x = i % 6
      val y = i / 6
      val offset = Vector2(x, y) * 50.0
      val color = Color.rgb(0xffffff)
      val alpha = math.sin(time * 5.0 + i * 7.0) * 0.25 + 0.75
      val col = color.copy(a = alpha)
      sb.draw(Identifier(s"sprites/$sprite.png"), Vector2(800.0, 100.0) + offset, Vector2(50.0, 50.0), col)
    }

    sb.flush()

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
