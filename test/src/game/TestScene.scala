package game

import java.io.File
import util.BufferUtils._
import java.nio.{ByteBuffer, ByteOrder}

import core._
import render._
import render.UniformBlock
import platform.AppWindow

object TestScene extends App {

  object ModelTextures extends SamplerBlock {
    val Diffuse = sampler2D("Diffuse", Sampler.RepeatAnisotropic)
  }

  object ModelUniform extends UniformBlock("ModelUniform") {
    val ViewProjection = mat4("ViewProjection")
    val World = mat4x3("World")
  }

  val VertexShader =
    """
#version 150

in vec3 a_Position;
in vec2 a_TexCoord;

layout(row_major)
layout(std140)
uniform ModelUniform {
  mat4 u_ViewProjection;
  mat4x3 u_World;
};

out vec2 v_TexCoord;

void main() {
  vec3 world = u_World * vec4(a_Position, 1.0);
  gl_Position = u_ViewProjection * vec4(world, 1.0);
  v_TexCoord = a_TexCoord;
}
    """

  val FragmentShader =
    """
#version 150

uniform sampler2D u_Diffuse;

in vec2 v_TexCoord;
out vec4 o_Color;

void main() {
  o_Color = texture(u_Diffuse, v_TexCoord);
}
    """

  val arg = util.ArgumentParser.parse(args)
  val debug = arg.flag("debug")

  AppWindow.initialize(1280, 720, "Test window", debug)

  val renderer = Renderer.initialize()
  val shader = Shader.compile(VertexShader, FragmentShader, ModelTextures, ModelUniform)

  val viewProjection = (
      Matrix4.perspective(1280.0 / 720.0, math.Pi / 3.0, 0.01, 1000.0)
      * Matrix43.look(Vector3(0.0, 0.0, -10.0), Vector3(0.0, 0.0, 1.0)))

  val buffer = ByteBuffer.allocateDirect(16 * 1024 * 1024)

  val mesh = {
    val file = new File("data/test/sausageman.fbx.Cube.000.s2ms")
    var buf = buffer.duplicateEx
    buf.order(ByteOrder.LITTLE_ENDIAN)
    buf.readFromFile(file)
    buf.finish()

    val mesh = new gfx.Mesh()
    mesh.load(buf)
    mesh
  }

  val texture = {
    val file = new File("data/test/grass.jpg.s2tx")
    var buf = buffer.duplicateEx
    buf.order(ByteOrder.LITTLE_ENDIAN)
    buf.readFromFile(file)
    buf.finish()

    val tex = new gfx.Texture()
    tex.load(buf)
    tex
  }

  var time = 0.0
  while (AppWindow.running) {
    AppWindow.pollEvents()

    renderer.setDepthMode(true, true)

    renderer.advanceFrame()

    time += 0.016
    val world = Matrix43.translate(0.0, -5.0, 0.0) * Matrix43.rotateY(time) * Matrix43.rotateX(math.Pi / 2.0)

    renderer.clear(Some(Color.rgb(0x6495ED)), Some(1.0))

    renderer.setShader(shader)

    renderer.pushUniform(ModelUniform, u => {
      import ModelUniform._
      ViewProjection.set(u, viewProjection)
      World.set(u, world)
    })

    renderer.setTexture(ModelTextures.Diffuse, texture.texture)

    for (part <- mesh.parts) {
      part.draw()
    }

    AppWindow.swapBuffers()
  }

  AppWindow.unload()
}
