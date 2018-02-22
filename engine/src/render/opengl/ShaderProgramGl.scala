package render.opengl

import org.lwjgl.system.MemoryStack

import scala.util.matching.Regex
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL20._
import org.lwjgl.opengl.GL30._
import org.lwjgl.opengl.GL31._

import render._
import ShaderProgramGl._

object ShaderProgramGl {

  case class UniformBind(serial: Int, shaderIndex: Int)
  case class AttribBind(semantic: VertexSpec.Semantic, index: Int, shaderIndex: Int)
  case class SamplerBind(index: Int, shaderIndex: Int)

  val AttribRegex = """^a_([A-Za-z]+)([0-9]*)$""".r

  object NullSamplers extends SamplerBlock

  def compile(vert: String, frag: String, uniforms: UniformBlock*): ShaderProgramGl = {
    compile(vert, frag, NullSamplers, uniforms : _*)
  }

  def compile(vert: String, frag: String, samplers: SamplerBlock, uniforms: UniformBlock*): ShaderProgramGl = {

    // -- Compile the shader
    def createShader(src: String, shaderType: Int): Int = {
      val shader = glCreateShader(shaderType)
      glShaderSource(shader, src)
      glCompileShader(shader)
      if (glGetShaderi(shader, GL_COMPILE_STATUS) != GL_TRUE) {
        val msg = glGetShaderInfoLog(shader)
        throw new ShaderCompileError(msg)
      }
      shader
    }

    val vs = createShader(vert, GL_VERTEX_SHADER)
    val fs = createShader(frag, GL_FRAGMENT_SHADER)
    val program = glCreateProgram()
    glAttachShader(program, vs)
    glAttachShader(program, fs)
    glLinkProgram(program)
    if (glGetProgrami(program, GL_LINK_STATUS) != GL_TRUE) {
      val msg = glGetProgramInfoLog(program)
      throw new ShaderCompileError(msg)
    }

    glDetachShader(program, vs)
    glDetachShader(program, fs)
    glDeleteShader(vs)
    glDeleteShader(fs)

    val stack = MemoryStack.stackPush()

    // -- List active uniform blocks
    val numUniformBlocks = glGetProgrami(program, GL_ACTIVE_UNIFORM_BLOCKS)
    val uniformMapping = (0 until numUniformBlocks).flatMap(index => {
      val name = glGetActiveUniformBlockName(program, index)
      uniforms.find(_.name == name).map(ub => UniformBind(ub.serial, index))
    }).toArray

    // -- List active attributes
    val numAttribs = glGetProgrami(program, GL_ACTIVE_ATTRIBUTES)
    val attribMapping = (0 until numAttribs).map(index => {
      val psize = stack.ints(0)
      val ptype = stack.ints(0)
      val name = glGetActiveAttrib(program, index, psize, ptype)
      name match {
        case AttribRegex(semStr, semIndexStr) =>
          val semIndex = if (semIndexStr.nonEmpty) semIndexStr.toInt else 0
          val sem = semStr match {
            case "Position" => VertexSpec.Semantic.Position
            case "TexCoord" => VertexSpec.Semantic.TexCoord
            case "TangentSpace" => VertexSpec.Semantic.TangentSpace
            case "BoneIndex" => VertexSpec.Semantic.BoneIndex
            case "BoneWeight" => VertexSpec.Semantic.BoneWeight
            case other => throw new ShaderCompileError(s"Not a valid vertex semantic: '$other'")
          }
          AttribBind(sem, semIndex, index)
        case _ => throw new ShaderCompileError(s"Attribute name '$name' doesn't match any semantic")
      }
    }).toArray

    // -- List active uniform textures
    val samplerMapping = samplers.samplers.flatMap(sampler => {
      val loc = glGetUniformLocation(program, "u_" + sampler.name)
      if (loc >= 0) {
        Some(SamplerBind(sampler.index, loc))
      } else {
        None
      }
    }).toArray

    stack.pop()

    val serial = serialCounter
    serialCounter += 1
    new ShaderProgramGl(serial, program, uniformMapping, attribMapping, samplerMapping)
  }

  private var serialCounter = 0

}

class ShaderProgramGl(val serial: Int,
                      val program: Int,
                      val uniforms: Array[UniformBind],
                      val attribs: Array[AttribBind],
                      val samplers: Array[SamplerBind]) {

  /**
    * Returns the index of a vertex attribute, -1 if not enabled
    */
  def getAttributeIndex(semantic: VertexSpec.Semantic, index: Int): Int = {
    var ix = 0
    while (ix < attribs.length) {
      val atr = attribs(ix)
      if ((atr.semantic eq semantic) && atr.index == index) return atr.shaderIndex
      ix += 1
    }
    -1
  }

}
