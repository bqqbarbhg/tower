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

  case class WindowStyle(val width: Int, val height: Int, val fullscreen: Boolean, val borderless: Boolean, val monitorIndex: Int)

  private var window: Long = NULL
  private var currentWindowStyle: WindowStyle = null

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

  /** Initialize the system */
  def initialize(): Unit = {
    assert(window == NULL)

    glfwInit()
  }

  /** Create the application window */
  def createWindow(style: WindowStyle, title: String, debug: Boolean): Unit = {

    var userSpecifiedMonitor: Long = NULL

    val maybeMonitor = if (style.monitorIndex > 0) {
      val ix = style.monitorIndex - 1
      val monitors = glfwGetMonitors()
      if (ix < monitors.capacity) {
        val monitor = monitors.get(ix)
        userSpecifiedMonitor = monitor
        monitor
      } else {
        glfwGetPrimaryMonitor()
      }
    } else {
      glfwGetPrimaryMonitor()
    }

    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3)
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3)
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE)
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE)

    if (!style.fullscreen)
      glfwWindowHint(GLFW_VISIBLE, GL_FALSE)

    var width = style.width
    var height = style.height

    if (style.borderless) {
      for {
        monitor <- Option(maybeMonitor)
        mode <- Option(glfwGetVideoMode(monitor))
      } {
        glfwWindowHint(GLFW_RED_BITS, mode.redBits)
        glfwWindowHint(GLFW_GREEN_BITS, mode.greenBits)
        glfwWindowHint(GLFW_BLUE_BITS, mode.blueBits)
        glfwWindowHint(GLFW_REFRESH_RATE, mode.refreshRate)
        width = mode.width
        height = mode.height
      }
    }


    val createMonitor = if (style.fullscreen) maybeMonitor else NULL

    if (debug)
      glfwWindowHint(GLFW_OPENGL_DEBUG_CONTEXT, GL_TRUE)

    window = glfwCreateWindow(width, height, title, createMonitor, NULL)
    glfwMakeContextCurrent(window)
    GL.createCapabilities()

    if (!style.fullscreen) {
      if (userSpecifiedMonitor != NULL) {
        val amonX = Array(0)
        val amonY = Array(0)
        glfwGetMonitorPos(userSpecifiedMonitor, amonX, amonY)

        for {
          mode <- Option(glfwGetVideoMode(userSpecifiedMonitor))
        } {
          val posX = (mode.width - style.width) / 2
          val posY = (mode.height - style.height) / 2
          glfwSetWindowPos(window, amonX(0) + posX, amonY(0) + posY)
        }
      }

      glfwShowWindow(window)
    }

    glfwSetKeyCallback(window, keyboard.keyCallback)

    if (debug && GL.getCapabilities.GL_KHR_debug) {
      OptsGl.useDebug = true
      glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS)
      glDebugMessageCallback(DebugListener, 0)
    }
  }

  /** Destroy the window */
  def destroyWindow(): Unit = {
    glfwDestroyWindow(window)
    window = NULL
  }

  /** Recreate the window if necessary (or create if never created before) */
  def recreateWindow(style: WindowStyle, title: String, debug: Boolean): Unit = {
    if (currentWindowStyle == style) return
    currentWindowStyle = style

    if (window != NULL)
      destroyWindow()

    createWindow(style, title, debug)
  }

  /** Set how many vertical syncs to wait between swaps (0 for no vsync) */
  def setSwapInterval(amount: Int): Unit = {
    glfwSwapInterval(amount)
  }

  /** Returns the refresh rate of the monitor the window is in */
  def monitorRefreshRate: Int = {
    val awinX = Array(0)
    val awinY = Array(0)
    glfwGetWindowPos(window, awinX, awinY)
    val winX = awinX(0)
    val winY = awinY(0)

    var windowMonitor = glfwGetWindowMonitor(window)
    if (windowMonitor == NULL) {
      windowMonitor = glfwGetPrimaryMonitor()
      val monitorList = glfwGetMonitors()
      for (i <- 0 until monitorList.capacity) {
        val monitor = monitorList.get(i)
        val amonX = Array(0)
        val amonY = Array(0)
        glfwGetMonitorPos(monitor, amonX, amonY)
        val monX = amonX(0)
        val monY = amonY(0)

        val mode = glfwGetVideoMode(monitor)
        if (mode != null) {
          if (winX >= monX && winY >= monY && winX < monX + mode.width && winY < monY + mode.height) {
            windowMonitor = monitor
          }
        }
      }
    }

    if (windowMonitor != NULL) {
      val mode = glfwGetVideoMode(windowMonitor)
      if (mode != null) {
        return mode.refreshRate
      }
    }

    60
  }

  /**
    * Represents an attached monitor.
    *
    * @param align String describing the alignment of the monitor.
    * @param pos Virtual desktop top-left position
    * @param size Virtual desktop size
    */
  case class MonitorInfo(val align: String, val pos: Vector2, val size: Vector2)

  /** List all attached monitors */
  def listMonitors: Seq[MonitorInfo] = {
    val monitorList = glfwGetMonitors()

    val coords = for (i <- 0 until monitorList.capacity) yield {
      val monitor = monitorList.get(i)
      val amonX = Array(0)
      val amonY = Array(0)
      glfwGetMonitorPos(monitor, amonX, amonY)
      Vector2(amonX(0), amonY(0))
    }

    val center = coords.fold(Vector2.Zero)(_ + _) / coords.length.toDouble
    val approxSize = math.max(center.x, center.y) * 2.0
    val epsilon = approxSize * 0.2

    val monitors = for (i <- 0 until monitorList.capacity) yield {
      val coord = coords(i)
      val monitor = monitorList.get(i)

      val dx = coord.x - center.x
      val dy = coord.y - center.y

      val adx = math.abs(dx)
      val ady = math.abs(dy)

      val align = if (math.max(adx, ady) >= epsilon * 0.5) {
        val relX = adx / (adx + ady)
        var xOff = 0
        var yOff = 0

        if (relX < 0.75) {
          yOff = if (dy < -epsilon) -1
          else if (dy > epsilon) 1
          else 0
        }

        if (relX > 0.25) {
          xOff = if (dx < -epsilon) -1
          else if (dx > epsilon) 1
          else 0
        }

        (xOff, yOff) match {
          case (-1, -1) => "TopLeft"
          case ( 1, -1) => "TopRight"
          case (-1,  1) => "BottomLeft"
          case ( 1,  1) => "BottomRight"
          case (-1,  0) => "Left"
          case ( 1,  0) => "Right"
          case ( 0, -1) => "Top"
          case ( 0,  1) => "Bottom"
          case ( 0,  0) => "Center"
          case _ => "Center"
        }
      } else {
        "Center"
      }

      val mode = glfwGetVideoMode(monitor)
      val size = if (mode != null) {
        Vector2(mode.width, mode.height)
      } else {
        Vector2(1920.0, 1080.0)
      }

      MonitorInfo(align, coord, size)
    }

    monitors.toSeq
  }

  /** Show the window (if created invisible) */
  def showWindow(): Unit = {
    glfwShowWindow(window)
  }

  /** Release resources */
  def unload(): Unit = {
    glfwTerminate()
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
