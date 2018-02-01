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
    val file = pack.get("test/sausageman.fbx.Cube.000.s2ms").get
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
      | layout(location = 1) in vec4 a_quat;
      |
      | uniform mat4 u_wvp;
      |
      | varying vec3 v_normal;
      |
      | void main() {
      |   gl_Position = u_wvp * vec4(a_pos, 1.0);
      |
      |   vec4 q = a_quat;
      |   vec3 normal;
      |   normal.x = 2.0 * (q.x*q.z - q.y*q.w);
      |   normal.y = 2.0 * (q.y*q.z + q.x*q.w);
      |   normal.z = 1.0 - 2.0 * (q.x*q.x + q.y*q.y);
      |   v_normal = normal;
      | }
      |
    """.stripMargin

  val FragmentShader =
    """
      | varying vec3 v_normal;
      |
      | void main() {
      |   float l = dot(normalize(v_normal), normalize(vec3(1.0, 1.0, 1.0)));
      |   float lc = clamp(l * 0.5 + 0.5, 0.0, 1.0);
      |   float llc = length(v_normal) * 0.5;
      |   gl_FragColor = vec4(vec3(lc), 1.0);
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

  glEnable(GL_DEPTH_TEST)

  glfwSwapInterval(1)

  val u_wvp = glGetUniformLocation(prog, "u_wvp")

  var time = 0.0

  // -- Main loop
  while ( !glfwWindowShouldClose(window) ) {

    time += 0.016

    glClearColor(0x64 / 255.0f, 0x95 / 255.0f, 0xED / 255.0f, 1.0f)
    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT)

    val proj = Matrix4.perspective(1280.0/720.0, scala.math.Pi / 2.0, 0.01, 1000.0)
    val world = Matrix4.rotateY(time) * Matrix4.rotateX(math.Pi / 2.0)
    val view = Matrix4.look(Vector3(0.0, 0.0, -10.0), Vector3(0.0, 0.0, 1.0))

    val wvp = proj * view * world

    for (part <- mesh.parts) {
      glBindVertexArray(part.vertexArray)
      glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, part.indexBuffer)

      glUseProgram(prog)
      val arr = new Array[Float](16)
      wvp.store(arr)
      glUniformMatrix4fv(u_wvp, false, arr)

      glDrawElements(GL_TRIANGLES, part.numIndices, GL_UNSIGNED_SHORT, 0)
    }

    glfwSwapBuffers(window)
    glfwPollEvents()
  }

  glfwDestroyWindow(window)
  glfwTerminate()

}
