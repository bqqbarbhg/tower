package platform

import org.lwjgl.glfw.GLFW._
import org.lwjgl.opengl.GL
import org.lwjgl.opengl.GL11._
import org.lwjgl.system.MemoryUtil.NULL
import org.lwjgl.opengl.ARBDebugOutput._
import org.lwjgl.opengl.GLDebugMessageARBCallbackI
import org.lwjgl.system.MemoryUtil

object AppWindow {

  private var window: Long = NULL

  object DebugListener extends GLDebugMessageARBCallbackI {
    override def invoke(source: Int, msgType: Int, id: Int, severity: Int,
                        length: Int, messagePointer: Long, userParam: Long): Unit = {

      val msgBuf = MemoryUtil.memByteBuffer(messagePointer, length)
      val bytes = new Array[Byte](length)
      msgBuf.get(bytes)
      val message = new String(bytes, "UTF-8")

      if (msgType == GL_DEBUG_TYPE_ERROR_ARB) {
        throw new RuntimeException(s"OpenGL error: $message")
      }
    }
  }

  /** Create the application window */
  def initialize(width: Int, height: Int, title: String, debug: Boolean): Unit = {
    assert(window == NULL)

    glfwInit()

    glfwWindowHint(GLFW_SAMPLES, 4)
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3)
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 2)
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE)
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE)

    if (debug)
      glfwWindowHint(GLFW_OPENGL_DEBUG_CONTEXT, GL_TRUE)

    window = glfwCreateWindow(1280, 720, "Hello World!", NULL, NULL)
    glfwMakeContextCurrent(window)
    GL.createCapabilities()
    glfwSwapInterval(1)

    if (debug && GL.getCapabilities.GL_KHR_debug) {
      glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS_ARB)
      glDebugMessageCallbackARB(DebugListener, 0)
    }
  }

  /** Release resources */
  def unload(): Unit = {
    glfwDestroyWindow(window)
    glfwTerminate()
  }

  /** Does the application window want to close? */
  def running: Boolean = !glfwWindowShouldClose(window)

  /** Update input etc */
  def pollEvents(): Unit = {
    glfwPollEvents()
  }

  /** Present the rendered frame */
  def swapBuffers(): Unit = {
    glfwSwapBuffers(window)
  }

}