package gfx

import java.nio.ByteBuffer

import core._
import render._
import util.BufferUtils._
import Shader._
import io.content.Package
import org.lwjgl.system.MemoryUtil

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Shader {

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

    def vert(name: String, values: Iterable[Int]) = push(MaskVert, name, values)
    def frag(name: String, values: Iterable[Int]) = push(MaskFrag, name, values)
    def both(name: String, values: Iterable[Int]) = push(MaskVert | MaskFrag, name, values)
  }

  object NoPermutations extends Permutations

  /** Simple wrapper class for setting permutation values */
  class PermutationSetter(val values: Array[Int]) extends AnyVal {

    def update(permutation: Permutation, value: Int): Unit = {
      values(permutation.index) = value
    }

    def update(permutation: Permutation, value: Boolean): Unit = {
      values(permutation.index) = if (value) 1 else 0
    }

  }

  private def loadShaderSource(filename: String): String = withStack {
    val file = Package.get.get(filename).getOrElse {
      throw new RuntimeException(s"Shader not found: $filename")
    }

    val buffer = alloca(file.sizeInBytes.toInt)
    val stream = file.read()
    buffer.readFrom(stream)
    stream.close()
    buffer.finish()

    // @Deserialize(s2sh)
    val MaxVersion = 1
    buffer.verifyMagic("s2sh")
    val version = buffer.getVersion(MaxVersion)
    val source = buffer.getString()
    buffer.verifyMagic("E.sh")

    source
  }

  /**
    * Load a shader from the content data.
    *
    * @param name Name of the shader to load
    * @param permutations Permutations to generate for the shader
    * @param samplers Sampler block passed to the renderer
    * @param uniforms Uniform blocks passed to the renderer
    * @return The compiled shader object
    */
  def load(name: String, permutations: Permutations, samplers: SamplerBlock, uniforms: UniformBlock*): Shader = {
    val perms = permutations.permutations

    // TODO: Generate less shaders using separable shader objects

    val vertSrc = loadShaderSource(name + ".vs.glsl.s2sh")
    val fragSrc = loadShaderSource(name + ".fs.glsl.s2sh")

    /** Generate the source for a shader permutation */
    def generateShaderPermutationSource(source: String, mask: Int, values: Seq[Int]): String = {
      val builder = new mutable.StringBuilder()

      // General shader header
      builder ++= "#version 330\n"

      // Uniform block macros
      if (render.opengl.OptsGl.useUniformBlocks) {
        builder ++= "#define UboBegin(p_name) layout(std140) layout(row_major) uniform p_name {\n"
        builder ++= "#define Ubo\n"
        builder ++= "#define UboMat layout(row_major)\n"
        builder ++= "#define UboEnd() };\n"
        builder ++= "layout(row_major) uniform;\n"
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

      // Reset the line for the actual source
      builder ++= "#line 1\n"

      // The actual source code
      builder ++= source

      builder.mkString
    }

    /** Compile a permutation shader program, which consists of vertex and fragment shaders */
    def compilePermutation(values: Seq[Int]): ShaderProgram = {
      val vertGen = generateShaderPermutationSource(vertSrc, MaskVert, values)
      val fragGen = generateShaderPermutationSource(fragSrc, MaskFrag, values)

      ShaderProgram.compile(vertGen, fragGen, samplers, uniforms : _*)
    }

    // Recursively generate all the permutations for the shader
    val permMap = new mutable.HashMap[Seq[Int], ShaderProgram]()
    def makePerms(index: Int, values: Vector[Int]): Unit = {
      if (index == perms.length) {
        val perm = values.toArray.toSeq
        permMap(perm) = compilePermutation(values)
      } else {
        for (value <- perms(index).values) {
          makePerms(index + 1, values :+ value)
        }
      }
    }

    makePerms(0, Vector[Int]())


    new Shader(permutations, permMap.toMap)
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
    val values = Array.fill(permutations.permutations.length)(PermNotSetMagic)
    setPerms(new PermutationSetter(values))
    val program = programs.getOrElse(values.toSeq, throwPermutationError(values))
    Renderer.get.setShader(program)
  }

  /** Enable the program to be used in further draw-calls. Version for shaders
    * without permutations */
  def use(): Unit = use(p => ())

}
