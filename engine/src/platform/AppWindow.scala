package platform

import org.lwjgl.glfw.GLFW._
import org.lwjgl.opengl.GL
import org.lwjgl.opengl.GL11._
import org.lwjgl.system.MemoryUtil.NULL

object AppWindow {

  private var window: Long = NULL

  /** Create the application window */
  def initialize(width: Int, height: Int, title: String): Unit = {
    assert(window == NULL)

    glfwInit()

    glfwWindowHint(GLFW_SAMPLES, 4)
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3)
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 2)
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE)
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE)

    window = glfwCreateWindow(1280, 720, "Hello World!", NULL, NULL)
    glfwMakeContextCurrent(window)
    GL.createCapabilities()
    glfwSwapInterval(1)
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
