package object render {

  type Renderer = render.opengl.RendererGl
  type Shader = render.opengl.ShaderProgramGl
  type VertexBuffer = render.opengl.VertexBufferGl
  type IndexBuffer = render.opengl.IndexBufferGl
  type TextureHandle = render.opengl.TextureHandleGl

  val Renderer = render.opengl.RendererGl
  val ShaderProgram = render.opengl.ShaderProgramGl
  val VertexBuffer = render.opengl.VertexBufferGl
  val IndexBuffer = render.opengl.IndexBufferGl
  val TextureHandle = render.opengl.TextureHandleGl

}
