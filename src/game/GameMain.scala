package tower.game

import java.util

import tower.engine.render._
import tower.engine.file._
import tower.util.SharedByteBuffer
import tower.util.Serialization.ByteBufferExtension
import org.lwjgl._
import org.lwjgl.glfw._
import org.lwjgl.glfw.Callbacks._
import org.lwjgl.glfw.GLFW._
import org.lwjgl.system.MemoryStack._
import org.lwjgl.system.MemoryUtil._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl.GL20._
import org.lwjgl.opengl.GL30._
import org.lwjgl.BufferUtils
import tower.engine.audio.platform.JavaAudioOutput
import tower.engine.audio.AudioOutput
import tower.engine.render
import tower.util.BufferUtils._
import tower.util.Serialization.ByteBufferExtension
import tower.math._

import scala.io.StdIn


object GameMain extends App {

  // -- Setup packages
  val pack = new MultiPackage()
  pack.add(new DirectoryPackage("data"), 0)
  // pack.add(new ZipFilePackage("mods/wat.zip"), -1)

  JarPackage.create("data") match {
    case Some(jar) => pack.add(jar, 1)
    case None => // Nop
  }

  // -- Initialize LWJ/GL/FW
  glfwInit()

  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3)
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 2)
  glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE)
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE)

  val window = glfwCreateWindow(1280, 720, "Hello World!", NULL, NULL)
  glfwMakeContextCurrent(window)
  org.lwjgl.opengl.GL.createCapabilities()

  // -- Load content
  val animation = {
    val anim = new Animation()
    val buf = SharedByteBuffer.acquire()
    val file = pack.get("test/sausageman.Walk.s2an").get
    val stream = file.read()
    buf.readFrom(stream)
    stream.close()
    anim.load(buf)
    SharedByteBuffer.release(buf)
    anim
  }
  val mesh = {
    val mesh = new Mesh()
    val buf = SharedByteBuffer.acquire()
    val file = pack.get("test/sausageman.Cube.000.s2ms").get
    val stream = file.read()
    buf.readFrom(stream)
    stream.close()
    mesh.load(buf)
    SharedByteBuffer.release(buf)
    mesh
  }

  val model = {
    val model = new Model()
    val buf = SharedByteBuffer.acquire()
    val file = pack.get("test/sausageman.s2md").get
    val stream = file.read()
    buf.readFrom(stream)
    stream.close()
    model.load(buf)
    SharedByteBuffer.release(buf)
    model
  }

  val texture = {
    val texture = new Texture()
    val buf = SharedByteBuffer.acquire()
    val file = pack.get("test/grass.s2tx").get
    val stream = file.read()
    buf.readFrom(stream)
    stream.close()
    texture.load(buf)
    SharedByteBuffer.release(buf)
    texture
  }

  val VertexShader =
    """
 #extension GL_ARB_explicit_attrib_location : enable

 layout(location = 0) in vec3 a_pos;
 layout(location = 1) in vec2 a_texCoord;
 layout(location = 2) in vec4 a_quat;
 layout(location = 3) in ivec4 a_bone;
 layout(location = 4) in vec4 a_weight;

 uniform mat4 u_wvp;
 uniform mat4 u_bones[12];

 varying vec3 v_normal;
 varying vec2 v_texCoord;

 void main() {
 #if 1
   mat4 xform =
     u_bones[a_bone.x] * a_weight.x +
     u_bones[a_bone.y] * a_weight.y +
     u_bones[a_bone.z] * a_weight.z +
     u_bones[a_bone.w] * a_weight.w ;
 #else
   mat4 xform = u_bones[a_bone.x];
 #endif

   gl_Position = u_wvp * (xform * vec4(a_pos, 1.0));

   vec4 q = a_quat;
   vec3 normal;
   normal.x = 2.0 * (q.x*q.z - q.y*q.w);
   normal.y = 2.0 * (q.y*q.z + q.x*q.w);
   normal.z = 1.0 - 2.0 * (q.x*q.x + q.y*q.y);
   v_normal = normalize((xform * vec4(normal, 0.0)).xyz);
   v_texCoord = a_texCoord;
 }

    """

  val FragmentShader =
    """
      | varying vec3 v_normal;
      | varying vec2 v_texCoord;
      |
      | uniform sampler2D u_texture;
      |
      | void main() {
      |   float l = dot(normalize(v_normal), normalize(vec3(-1.0, 1.0, -1.0)));
      |   float lc = clamp(l * 0.5 + 0.5, 0.0, 1.0);
      |   vec3 color = texture2D(u_texture, v_texCoord).rgb;
      |   gl_FragColor = vec4(color * lc, 1.0);
      | }
      |
    """.stripMargin

  val animState = new AnimationState(model)

  val vert = glCreateShader(GL_VERTEX_SHADER)
  val frag = glCreateShader(GL_FRAGMENT_SHADER)
  val prog = glCreateProgram()
  glShaderSource(vert, VertexShader)
  glShaderSource(frag, FragmentShader)
  glAttachShader(prog, vert)
  glAttachShader(prog, frag)
  glCompileShader(vert)
  glCompileShader(frag)
  println("Vert: " + glGetShaderInfoLog(vert))
  println("Frag: " + glGetShaderInfoLog(frag))
  glLinkProgram(prog)
  println(glGetProgramInfoLog(prog))

  glEnable(GL_DEPTH_TEST)
  glEnable(GL_CULL_FACE)
  glFrontFace(GL_CW)

  glfwSwapInterval(1)

  val SampleRate = 44200
  val Chunk = 4000
  val audioOutput: AudioOutput = new JavaAudioOutput(SampleRate)
  val samples = new Array[Short](Chunk * 2)

  var runAudio: Boolean = true

  val audioThread = new Thread() {
    override def run(): Unit = {
      val begin = System.currentTimeMillis()
      var soundTime = 0
      while (runAudio) {
        val realTime = ((System.currentTimeMillis() - begin).toDouble / 1000.0 * SampleRate).toInt
        val toWrite = math.min(realTime - soundTime, Chunk)

        for (i <- 0 until toWrite) {
          val t = (soundTime + i).toDouble / SampleRate
          val s = math.sin(440.0 * math.Pi * 2.0 * t) * 0.1
          val u = (s * Short.MaxValue.toDouble).toShort
          samples(i * 2 + 0) = u
          samples(i * 2 + 1) = u
        }
        soundTime += toWrite

        audioOutput.write(samples, toWrite * 2)
      }
    }
  }
  audioThread.start()

  val u_wvp = glGetUniformLocation(prog, "u_wvp")
  val u_bones = glGetUniformLocation(prog, "u_bones")
  val u_texture = glGetUniformLocation(prog, "u_texture")

  var time = 0.0

  val arr = BufferUtils.createFloatBuffer(16 * 12)

  val animLayer = animState.addAnimation(animation)

  // -- Main loop
  while ( !glfwWindowShouldClose(window) ) {


    time += 0.016
    animLayer.time = time % animation.duration

    glClearColor(0x64 / 255.0f, 0x95 / 255.0f, 0xED / 255.0f, 1.0f)
    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT)

    val proj = Matrix4.perspective(1280.0/720.0, scala.math.Pi / 2.5, 0.01, 1000.0)
    val world = Matrix43.rotateY(4.0 + time * 0.05) * Matrix43.scale(0.01)
    val view = Matrix4.look(Vector3(0.0, 5.0, -10.0), Vector3(0.0, 0.0, 1.0))

    val wvp = proj * view

    animState.worldTransform = world
    animState.apply()

    for (part <- mesh.parts) {

      glUseProgram(prog)

      glBindTexture(GL_TEXTURE_2D, texture.textureHandle)

      glUniform1i(u_texture, 0)

      wvp.store(arr)
      if (u_wvp >= 0)
        glUniformMatrix4fv(u_wvp, false, arr.limited(16))

      for (index <- 0 until part.numBones) {
        val nodeIndex = model.findNodeByName(part.boneNames(index))
        val matrix = animState.transform(nodeIndex) * part.boneMeshToBone(index)
        matrix.store(arr, index * 16)
      }

      if (u_bones >= 0)
        glUniformMatrix4fv(u_bones, false, arr.limited(16 * part.numBones))

      glBindVertexArray(part.vertexArray)
      glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, part.indexBuffer)

      glDrawElements(GL_TRIANGLES, part.numIndices, GL_UNSIGNED_SHORT, 0)
    }

    glfwSwapBuffers(window)
    glfwPollEvents()
  }

  glfwDestroyWindow(window)
  glfwTerminate()

  audioOutput.close()
  runAudio = false
  audioThread.join()
}
