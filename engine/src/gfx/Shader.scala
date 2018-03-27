package gfx

import java.nio.ByteBuffer

import core._
import render._
import util.BufferUtils._
import Shader._
import task.Task
import io.content.Package
import org.lwjgl.system.MemoryUtil
import render.opengl.OptsGl
import render.opengl.ShaderProgramGl.SourceChunk

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Shader {

  val PreludeFilename = Identifier("<permutation-prelude>")

  val MaskVert = 0x01
  val MaskFrag = 0x02

  val PermNotSetMagic = 0xF00DDEAD

  /**
    * A permutation of the shader with different '#define' set.
    *
    * @param mask Shader types to use the permutation for (eg. MaskVert, MaskFrag)
    * @param name Name of the '#define' to set
    * @param values Set of values for the #define
    * @param index Identifying index of the permutation
    */
  case class Permutation(mask: Int, name: String, values: Set[Int], index: Int)

  /**
    * A static define used in the shader.
    *
    * @param mask Shader types to use the define for (eg. MaskVert, MaskFrag)
    * @param name Name of the '#define' to set
    * @param value Value of the define
    */
  case class Define(mask: Int, name: String, value: () => Int)

  /** A collection of shader permutations */
  class Permutations {
    var permutations = new ArrayBuffer[Permutation]()

    private def push(mask: Int, name: String, values: Iterable[Int]): Permutation = {
      val valueSet = values.toSet
      assert(!valueSet.contains(PermNotSetMagic), "Permutation contains unset permutation magic value, change the magic!")
      val perm = new Permutation(mask, name, valueSet, permutations.length)
      permutations += perm
      perm
    }

    def vert(name: String, values: Iterable[Int]): Permutation = push(MaskVert, name, values)
    def frag(name: String, values: Iterable[Int]): Permutation = push(MaskFrag, name, values)
    def both(name: String, values: Iterable[Int]): Permutation = push(MaskVert | MaskFrag, name, values)
  }

  /** A collection of shader defines */
  class Defines {
    var defines = new ArrayBuffer[Define]()

    private def push(mask: Int, name: String, value: => Int): Unit = {
      defines += new Define(mask, name, () => value)
    }

    def vert(name: String, value: => Int): Unit = push(MaskVert, name, value)
    def frag(name: String, value: => Int): Unit = push(MaskFrag, name, value)
    def both(name: String, value: => Int): Unit = push(MaskVert | MaskFrag, name, value)
  }

  object NoPermutations extends Permutations
  object NoDefines extends Defines

  /** Simple wrapper class for setting permutation values */
  class PermutationSetter(val values: Array[Int]) extends AnyVal {

    def update(permutation: Permutation, value: Int): Unit = {
      values(permutation.index) = value
    }

    def update(permutation: Permutation, value: Boolean): Unit = {
      values(permutation.index) = if (value) 1 else 0
    }

  }

  /**
    * Load a shader from the content data.
    *
    * @param name Name of the shader (for debugging)
    * @param vertSrc Source of the vertex shader
    * @param fragSrc Source of the fragment shader
    * @param permutations Permutations to generate for the shader
    * @param samplers Sampler block passed to the renderer
    * @param uniforms Uniform blocks passed to the renderer
    * @return The compiled shader object
    */
  def deferredLoad(name: String, vertSrc: ShaderSource, fragSrc: ShaderSource, permutations: Permutations, defines: Defines, samplers: SamplerBlock, uniforms: UniformBlock*): Task[Shader] = {
    val perms = permutations.permutations

    // TODO: Generate less shaders using separable shader objects

    /** Generate the source for a shader permutation */
    def generateShaderPermutationPrelude(mask: Int, values: Seq[Int]): SourceChunk = {
      val builder = new mutable.StringBuilder()

      // General shader header
      builder ++= "#version 330\n"

      // Uniform block macros
      if (render.opengl.OptsGl.useUniformBlocks) {
        if (OptsGl.useUboStd140)
          builder ++= "#define UboBegin(p_name) layout(std140, row_major) uniform p_name {\n"
        else {
          if (OptsGl.useRowMajorMatrix) {
            builder ++= "#define UboBegin(p_name) layout(shared, row_major) uniform p_name {\n"
          } else {
            builder ++= "#define UboBegin(p_name) layout(shared) uniform p_name {\n"
          }
        }
        builder ++= "#define Ubo\n"
        if (!OptsGl.useUboStd140 && OptsGl.useRowMajorMatrix) {
          builder ++= "#define UboMat layout(row_major)\n"
        } else {
          builder ++= "#define UboMat\n"
        }
        builder ++= "#define UboEnd() };\n"
        if (!OptsGl.useUboStd140 && OptsGl.useRowMajorMatrix) {
          builder ++= "layout(row_major) uniform;\n"
        }
      } else {
        builder ++= "#define UboBegin(p_name)\n"
        builder ++= "#define Ubo uniform\n"
        builder ++= "#define UboMat uniform\n"
        builder ++= "#define UboEnd()\n"
      }

      // Permutation defines
      for ((perm, value) <- (perms zip values)) {
        if ((perm.mask & mask) != 0) {
          builder ++= s"#define ${perm.name} $value\n"
        }
      }

      // Static defines
      for (define <- defines.defines) {
        if ((define.mask & mask) != 0) {
          val value = define.value()
          builder ++= s"#define ${define.name} $value\n"
        }
      }

      SourceChunk(PreludeFilename, 1, builder.mkString)
    }

    /** Compile a permutation shader program, which consists of vertex and fragment shaders */
    def compilePermutation(values: Seq[Int]): Option[ShaderProgram] = {
      try {
        val vertGen = generateShaderPermutationPrelude(MaskVert, values)
        val fragGen = generateShaderPermutationPrelude(MaskFrag, values)

        val vertChunks = vertGen +: vertSrc.chunks
        val fragChunks = fragGen +: fragSrc.chunks

        val program = ShaderProgram.compile(vertChunks, fragChunks, samplers, uniforms : _*)

        val permutationDebug = (for ((value, perm) <- (values zip perms)) yield {
          s"${perm.name}:$value"
        }).mkString(" ")

        program.setLabel(s"$name ($permutationDebug)")

        Some(program)
      } catch {
        case e: ShaderCompileError =>
          println(e.getMessage)
          None
      }
    }

    // Recursively generate all the permutations for the shader
    val permMap = new mutable.HashMap[Seq[Int], Task[(Seq[Int], Option[ShaderProgram])]]()
    def makePerms(index: Int, values: Vector[Int]): Unit = {
      if (index == perms.length) {
        val perm = values.toArray.toSeq
        val programTask = Task.Main.add(() => {
          (perm, compilePermutation(values))
        })
        permMap(perm) = programTask
      } else {
        for (value <- perms(index).values) {
          makePerms(index + 1, values :+ value)
        }
      }
    }

    makePerms(0, Vector[Int]())

    val mainTask = Task.Main.add(permMap.values.toSeq, (permResults: Seq[(Seq[Int], Option[ShaderProgram])]) => {
      val resolvedPerms = permResults.toMap
      if (resolvedPerms.valuesIterator.exists(_.isEmpty)) {
        for (op <- resolvedPerms.valuesIterator; p <- op) {
          p.unload()
        }
        new Shader(permutations, Map[Seq[Int], ShaderProgram]())
      } else {
        new Shader(permutations, resolvedPerms.mapValues(_.get))
      }
    })

    mainTask
  }

  def load(name: String, vertSrc: ShaderSource, fragSrc: ShaderSource, permutations: Permutations, defines: Defines, samplers: SamplerBlock, uniforms: UniformBlock*): Shader = {
    deferredLoad(name, vertSrc, fragSrc, permutations, defines, samplers, uniforms : _*).get
  }
}

class Shader(val permutations: Permutations, val programs: Map[Seq[Int], ShaderProgram]) {

  /** Throw a useful error for setting the wrong or missing value for permutations */
  private def throwPermutationError(values: Array[Int]): Nothing = {
    for ((value, perm) <- (values zip permutations.permutations)) {
      if (value == PermNotSetMagic) {
        throw new RuntimeException(s"Permutation ${perm.name} was not set!")
      } else if (!perm.values.contains(value)) {
        throw new RuntimeException(s"Permutation ${perm.name} does not support value $value!")
      }
    }
    throw new AssertionError("Permutation was not found, but all the values were ok")
  }

  /**
    * Enable the program to be used in further draw-calls.
    *
    * @param setPerms Callback for setting the permutations.
    */
  def use(setPerms: PermutationSetter => Unit): Unit = {
    if (programs.isEmpty) {
      Renderer.get.setInvalidShader()
      return
    }

    val values = Array.fill(permutations.permutations.length)(PermNotSetMagic)
    setPerms(new PermutationSetter(values))
    val program = programs.getOrElse(values.toSeq, throwPermutationError(values))
    Renderer.get.setShader(program)
  }

  /** Enable the program to be used in further draw-calls. Version for shaders
    * without permutations */
  def use(): Unit = use(p => ())

  /** Free the resources used by the shader */
  def unload(): Unit = {
    for ((perm, program) <- programs) {
      program.unload()
    }
  }

}
