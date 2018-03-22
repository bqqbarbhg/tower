package platform

import core._
import input.device.Keyboard
import org.lwjgl.glfw.GLFW._
import org.lwjgl.opengl.GL
import org.lwjgl.opengl.GL11._
import org.lwjgl.system.MemoryUtil.NULL
import org.lwjgl.opengl.KHRDebug._
import org.lwjgl.opengl.GLDebugMessageCallbackI
import org.lwjgl.system.MemoryUtil
import render.opengl.OptsGl

object AppWindow {

  private var window: Long = NULL

  object DebugListener extends GLDebugMessageCallbackI {
    override def invoke(source: Int, msgType: Int, id: Int, severity: Int,
                        length: Int, messagePointer: Long, userParam: Long): Unit = {

      val msgBuf = MemoryUtil.memByteBuffer(messagePointer, length)
      val bytes = new Array[Byte](length)
      msgBuf.get(bytes)
      val message = new String(bytes, "UTF-8")

      if (msgType == GL_DEBUG_TYPE_ERROR) {
        throw new RuntimeException(s"OpenGL error: $message")
      } else if (msgType != GL_DEBUG_TYPE_OTHER) {
        println(s"GL: $message")
      }
    }
  }

  val keyboard = new Keyboard()

  /** Create the application window */
  def initialize(width: Int, height: Int, title: String, debug: Boolean): Unit = {
    assert(window == NULL)

    glfwInit()

    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3)
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3)
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE)
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE)

    if (debug)
      glfwWindowHint(GLFW_OPENGL_DEBUG_CONTEXT, GL_TRUE)

    window = glfwCreateWindow(width, height, title, NULL, NULL)
    glfwMakeContextCurrent(window)
    GL.createCapabilities()
    glfwSwapInterval(1)

    glfwSetKeyCallback(window, keyboard.keyCallback)

    if (debug && GL.getCapabilities.GL_KHR_debug) {
      OptsGl.useDebug = true
      glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS)
      glDebugMessageCallback(DebugListener, 0)
    }
  }

  /** Show the window (if created invisible) */
  def showWindow(): Unit = {
    glfwShowWindow(window)
  }

  /** Release resources */
  def unload(): Unit = {
    glfwDestroyWindow(window)
    glfwTerminate()
    window = NULL
  }

  /** Does the application window want to close? */
  def running: Boolean = !glfwWindowShouldClose(window)

  /** Update input etc */
  def pollEvents(): Unit = {
    glfwPollEvents()
    keyboard.update()
  }

  /** Present the rendered frame */
  def swapBuffers(): Unit = {
    glfwSwapBuffers(window)
  }

  /** Width of the window inner area in pixels */
  def width: Int = {
    val x = Array(0)
    val y = Array(0)
    glfwGetFramebufferSize(window, x, y)
    x(0)
  }

  /** Height of the window inner area in pixels */
  def height: Int = {
    val x = Array(0)
    val y = Array(0)
    glfwGetFramebufferSize(window, x, y)
    y(0)
  }

  /** Returns the pixel position of the mouse inside the window area */
  def mousePosition: Vector2 = {
    val x = Array(0.0)
    val y = Array(0.0)
    glfwGetCursorPos(window, x, y)
    Vector2(x(0), y(0))
  }

  /** Returns whether a mouse button is currently down */
  def mouseButtonDown(index: Int): Boolean = {
    glfwGetMouseButton(window, index) == GLFW_PRESS
  }

  def currentTime: Double = glfwGetTime
}
