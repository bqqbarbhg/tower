package platform

import core._
import org.lwjgl.glfw.GLFW._
import org.lwjgl.glfw.GLFWKeyCallbackI
import org.lwjgl.glfw.GLFWCharCallbackI
import org.lwjgl.glfw.GLFWScrollCallbackI
import org.lwjgl.opengl.GL
import org.lwjgl.opengl.GL11._
import org.lwjgl.system.MemoryUtil.NULL
import org.lwjgl.opengl.KHRDebug._
import org.lwjgl.opengl.GLDebugMessageCallbackI
import org.lwjgl.system.MemoryUtil
import render.opengl.OptsGl

import scala.collection.mutable.ArrayBuffer

object AppWindow {

  case class WindowStyle(val width: Int, val height: Int, val fullscreen: Boolean, val borderless: Boolean, val monitorIndex: Int, val icon: Option[IconImage])

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

  /**
    * GLFW callbacks may be called outside the `glfwPollEvents()` function.
    * This is terrifying as we don't even necessary know which thread will call
    * them. So buffer the key events in this way:
    * - GLFW callbacks always write to `glfwCallbackQueue`
    * - Application reads from `readQueue`
    * - Once a frame, after `glfwPollEvents()` GLFW queue is copied to the read queue (synchronized)
    *
    * This results in no extra latency in normally posted key events (in the
    * event polling function) as the queue is copied on the same frame the events
    * are received. Events posted from questionable sources will be safely buffered
    * until the next frame.
    */
  private class KeyboardEventQueue {
    val keyEvents = new ArrayBuffer[KeyEvent]()
    val charEvents = new ArrayBuffer[CharEvent]()
    var downEvents = new ArrayBuffer[KeyEvent]()

    var mouseScroll: Double = 0.0
  }

  private val glfwCallbackQueue = new KeyboardEventQueue()
  private val readQueue = new KeyboardEventQueue()

  private val keyCallback = new GLFWKeyCallbackI {
    override def invoke(window: Long, key: Int, scancode: Int, action: Int, mods: Int): Unit = {
      glfwCallbackQueue.synchronized {
        glfwCallbackQueue.keyEvents += KeyEvent(key, action, mods)
      }
    }
  }

  private val charCallback = new GLFWCharCallbackI {
    override def invoke(window: Long, codepoint: Int): Unit = {
      glfwCallbackQueue.synchronized {
        glfwCallbackQueue.charEvents += new CharEvent(codepoint)
      }
    }
  }

  private val scrollCallback = new GLFWScrollCallbackI {
    override def invoke(window: Long, xoffset: Double, yoffset: Double): Unit = {
      glfwCallbackQueue.synchronized {
        glfwCallbackQueue.mouseScroll += yoffset
      }
    }
  }

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

    for (icon <- style.icon) {
      glfwSetWindowIcon(window, icon.images)
    }

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

    glfwSetKeyCallback(window, keyCallback)
    glfwSetCharCallback(window, charCallback)
    glfwSetScrollCallback(window, scrollCallback)

    if (debug && GL.getCapabilities.GL_KHR_debug) {
      OptsGl.useDebug = true
      glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS)
      glDebugMessageCallback(DebugListener, 0)
    }
  }

  /** Key events for the current frame */
  def keyEvents: Seq[KeyEvent] = readQueue.keyEvents

  /** Key events with `down == true` for the current frame */
  def keyDownEvents: Seq[KeyEvent] = readQueue.downEvents

  /** Character events for the current frame */
  def charEvents: Seq[CharEvent] = readQueue.charEvents

  /** Mouse scroll in click-increments */
  def mouseScroll: Double = readQueue.mouseScroll

  /** Is a key currently pressed down */
  def keyDown(key: Int): Boolean = glfwGetKey(window, key) == GLFW_PRESS

  /** Is any control key down */
  def modifierControlDown: Boolean = keyDown(GLFW_KEY_LEFT_CONTROL) || keyDown(GLFW_KEY_RIGHT_CONTROL)

  /** Is any shift key down */
  def modifierShiftDown: Boolean = keyDown(GLFW_KEY_LEFT_SHIFT) || keyDown(GLFW_KEY_RIGHT_SHIFT)

  /** Is any alt key down */
  def modifierAltDown: Boolean = keyDown(GLFW_KEY_LEFT_ALT) || keyDown(GLFW_KEY_RIGHT_ALT)

  /** Set text to system clipboard */
  def setClipboard(text: String): Unit = {
    glfwSetClipboardString(window, text)
  }

  /** Get text from system clipboard */
  def getClipboard: String = {
    glfwGetClipboardString(window)
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

    glfwCallbackQueue.synchronized {
      readQueue.keyEvents.clear()
      readQueue.charEvents.clear()
      readQueue.keyEvents ++= glfwCallbackQueue.keyEvents
      readQueue.charEvents ++= glfwCallbackQueue.charEvents
      readQueue.downEvents = readQueue.keyEvents.filter(_.down)
      readQueue.mouseScroll = glfwCallbackQueue.mouseScroll
      glfwCallbackQueue.keyEvents.clear()
      glfwCallbackQueue.charEvents.clear()
      glfwCallbackQueue.mouseScroll = 0.0
    }
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
