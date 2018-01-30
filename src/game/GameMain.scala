package tower.game

import java.util

import tower.engine.render.{Animation, Mesh}
import tower.engine.file.{DirectoryPackage, JarPackage, MultiPackage, ZipFilePackage}
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
import tower.math._

import scala.io.StdIn


object GameMain extends App {

  // -- Setup packages
  val pack = new MultiPackage()
  pack.add(new DirectoryPackage("data"), 0)
  pack.add(new ZipFilePackage("mods/wat.zip"), -1)

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
    val file = pack.get("test/test-tube.fbx.Sway.s2an").get
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
    val file = pack.get("test/test-tube.fbx.Cylinder.s2ms").get
    val stream = file.read()
    buf.readFrom(stream)
    stream.close()
    mesh.load(buf)
    SharedByteBuffer.release(buf)
    mesh
  }

  val VertexShader =
    """
      | #extension GL_ARB_explicit_attrib_location : enable
      |
      | layout(location = 0) in vec3 a_pos;
      | uniform mat4 u_wvp;
      |
      | void main() {
      |   gl_Position = u_wvp * vec4(a_pos, 1.0);
      | }
      |
    """.stripMargin

  val FragmentShader =
    """
      |
      | void main() {
      |   gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0);
      | }
      |
    """.stripMargin

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

  glfwSwapInterval(1)

  val u_wvp = glGetUniformLocation(prog, "u_wvp")

  var time = 0.0

  // -- Main loop
  while ( !glfwWindowShouldClose(window) ) {

    time += 0.016 * 0.5

    glClearColor(0x64 / 255.0f, 0x95 / 255.0f, 0xED / 255.0f, 1.0f)
    glClear(GL_COLOR_BUFFER_BIT)

    val proj = Matrix4.perspective(1280.0/720.0, scala.math.Pi / 2.0, 0.01, 1000.0)
    val world = Matrix4.rotateY(time)
    val view = Matrix4.look(Vector3(0.0, 0.0, -20.0), Vector3(0.0, 0.0, 1.0))

    val wvp = proj * view * world

    glUseProgram(prog)
    val arr = new Array[Float](16)
    wvp.store(arr)
    glUniformMatrix4fv(u_wvp, false, arr)

    glDrawElements(GL_TRIANGLES, mesh.numIndices, GL_UNSIGNED_SHORT, 0)

    glfwSwapBuffers(window)
    glfwPollEvents()
  }

  glfwDestroyWindow(window)
  glfwTerminate()

}
