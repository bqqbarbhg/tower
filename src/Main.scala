import org.lwjgl._
import org.lwjgl.glfw._

import org.lwjgl.glfw.Callbacks._
import org.lwjgl.glfw.GLFW._
import org.lwjgl.opengl.GL11._
import org.lwjgl.system.MemoryStack._
import org.lwjgl.system.MemoryUtil._

object Main extends App {

  glfwInit()

  val window = glfwCreateWindow(300, 300, "Hello World!", NULL, NULL)

  glfwMakeContextCurrent(window)

  org.lwjgl.opengl.GL.createCapabilities()

  while ( !glfwWindowShouldClose(window) ) {

    glClearColor(0x64 / 255.0f, 0x95 / 255.0f, 0xED / 255.0f, 1.0f)
    glClear(GL_COLOR_BUFFER_BIT)

    glfwSwapBuffers(window)
    glfwPollEvents()
  }

  glfwDestroyWindow(window)
  glfwTerminate()

}

