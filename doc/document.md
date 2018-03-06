# Project document

## Asset pipeline

The game needs hand-crafted assets, such as sounds, textures, and models.
They are authored using some content creation tools and exported in exchange
formats like *.wav*, *.png*, and *.fbx* respectively. The engine could load
these formats directly, but it would be inefficent since they might have lots
of extra data that the game doesn't care about or be in a format that is very
expensive to convert into an usable form. Thus there needs to be preprocessing
of some sort, that converts the source assets into something the engine can
understand. These are custom formats and they have extensions with the prefix
*.s2XX* (named after the course, Studio 2), for example textures are processed
into *.s2tx* files.

![
Flowchart:
texture.png to texture.s2tx.
model.fbx to model.s2md.
sound.wav to sound.s2au.
](image/pipe-01-simple.png)&shy;

The simplest way to implement this processing is to go through all the assets
and convert them depending on their filetype. This works pretty well up to a
point. However, there are cases when you want to process the same file extension
in a different way depending on the context. If a *.png* file is used as a
texture on a model it should be processed in isolation, but if it is an UI
element it should be merged with others into a [sprite sheet][wiki-sprite-sheet].
You could hack around it for example by specifying textures as *.tga* files
and sprites as *.png* files, but this is just delaying the inevitable. The
real solution to the issue is to introduce some files for metadata and not
proecss the resources automatically. This metadata is stored as [*.toml*][gh-toml] files.

![
Flowchart:
texture.png to texture.toml to texture.s2tx.
sprite1.png and sprite2.png to atlas.toml to atlas.s2tx.
](image/pipe-02-metadata.png)&shy;

Even though the resources need different kinds of processing there are
significant processing steps that are shared between them. This is resolved
using an internal representation for the assets, which decouples the asset
importing from the processing. For example loaded image files are converted to __Image__
objects that contain the pixels of the image in an easy-to-manipulate way.
There is another internal format for image data called __Texture__. Unlike
__Image__, the data in them is *opaque* and cannot be operated upon. __Texture__
objects may contain [block compressed][wiki-texture-compression] data and
[mipmaps][wiki-mipmap]. All image data must be converted to __Texture__ before
saving it for the engine.

![
Flowchart:
texture.png to stb\_image to Image to Texture to texture.s2tx.
sprite1.png to stb\_image to Image.
sprite2.bmp to BmpImage to Image.
Images from sprite1 and sprite2 to Image to Texture to atlas.s2tx.
stb\_image and BmpImage are labeled Importer.
Image and Texture are labeled Internal.
](image/pipe-03-ir.png)&shy;

The processed results can be cached if the source files do not change between
processing runs. To avoid stale caches we need to keep track of all the source
files and metadata that influence each output file. When saving processed output
files a file containing the timestamps and hashes of the input files is saved
to a temporary cache directory mirroring the output structure. The processing
application compares this directory to the source files to determine which files
need to be reprocessed.

## OpenGL options

[OpenGL][about-opengl] is a standardized graphics API that the game uses to
render all its content. More specifically, the game requires the version 3.3
of the OpenGL API. OpenGL is quite a high-level API, which lends itself to
varying quality of driver implementations: Some patterns, even if correct in
the OpenGL API, are unbearably slow on actual implementations. Some features
may be even broken (for example issue #1). OpenGL also gives the freedom to
use features found in the newer versions given that you query that they are
actually available.

To work around implementation issues and take advantage of newer OpenGL features
the low-level rendering code accumulated lots of options collected in `OptsGl`.

### Uniform buffers

The primary way to pass data to shaders is to use [uniform][gl-uniform] variables.
As the name implies these are constant for all shader invocations in a single
draw call. For example let's take a simple vertex shader that has a few uniform
matrices:

```glsl
uniform mat4 u_World;
uniform mat4 u_ViewProj;
```

#### Old-style uniforms

![Values to glUniform() to internal UBO](image/ubo-01-old.png)&shy;

The old-school way to pass the uniform data to the shader is to query the
uniform locations and pass the data using the `glUniform()` family of calls:

```scala
// Query the locations of the uniforms (needs to be done only once)
val locWorld = glGetUniformLocation(program, "u_World")
val locViewProj = glGetUniformLocation(program, "u_ViewProj")

// Set the values using the uniform locations
glUseProgram(program);
glUniformMatrix4fv(locWorld, ...)
glUniformMatrix4fv(locViewProj, ...)
```


The advantage of this method is that it's pretty high-level, so it's easy to
use and the driver has a lot of the freedom to do optimizations. However, this
results in a lot of API calls, increased variance between different platforms,
and reduced ability to re-use uniforms between shaders and frames. For example
there is no way to store the uniforms for static objects between frames, instead
they have to be re-uploaded each frame.

#### Uniform Buffer Objects

![Mapped UBO passes through GL to explicit UBO](image/ubo-02-mapped.png)&shy;

Fortunately OpenGL 3.1 standardized [uniform buffer objects][gl-ubo], or UBO for
short. Uniform buffer objects are chunks of memory that can be read by shaders.
Applications may write the uniform values into them directly without any API
calls. Also they can be stored in memory and re-used as needed.

To use uniform buffers instead of plain uniforms the shader declarations must be
wrapped in an UBO block:
```glsl
uniform UBO {
	mat4 u_World;
	mat4 u_ViewProj;
};
```

The usage code is more complicated as it requires wrangling with a buffer, but
now all the per-uniform API calls are gone. Since this method is lower level
it leaves less chances for the GL driver to mess things up.

```scala
// Find the UBO and bind it to some (arbitrary) index
val UboBinding = 1
val index = glGetUniformBlockIndex(program, "UBO")
glUniformBlockBinding(program, index, UboBinding)

// Write the uniform data to the mapped buffer
glBindBuffer(GL_UNIFORM_BUFFER, buffer)
val data = glMapBufferRange(GL_UNIFORM_BUFFER, offset, size, ...)
writeMatrix(data, ...)
writeMatrix(data, ...)
glUnmapBuffer(GL_UNIFORM_BUFFER);

// Before doing a draw call bind the buffer
glBindBufferRange(GL_UNIFORM_BUFFER, UboBinding, buffer, offset, size)
```

The above example can be improved further by using [persistent mapping][gl-persistent],
which gets rid of nearly all the buffer mapping related calls. Uniform buffer
objects majorly sidesteps the OpenGL
driver and give the application a closer access to the GPU itself. Sadly UBOs
are not supported on WebGL 1.0 and GLES 2.0, which are still considerable targets.
Also the implementation may have some serious bugs as is in the case of some macOS
drivers.

#### Virtual UBOs

![Virtual UBO to glUniform() to internal UBO](image/ubo-03-virtual.png)&shy;

Instead of sacrificing UBOs for compatability, this engine takes another approach:
The uniforms are _always_ written to a buffer, even when not supported by the
platform. In case there is no support for uniform buffers, the buffer is a
"virtual UBO" that is just a chunk of application memory with a known layout.
The uniform values are later copied from this buffer using the `glUniform()` API.
Now the code for writing the uniforms is exactly the same no matter whether UBOs
are used or not: only the destination where they're written is different.

```scala
// Alternative implementations: Map buffer or allocate "virtual UBO"
val data = if (hasUboSupport) {
	glBindBuffer(GL_UNIFORM_BUFFER, buffer)
	glMapBufferRange(GL_UNIFORM_BUFFER, offset, size, ...)
} else {
	MemoryUtil.memAllocStack(size)
}

// Unified part: Write the uniforms into some UBO
writeMatrix(data, ...)
writeMatrix(data, ...)

// Alternative implementations: Bind UBO or upload uniforms one-by-one
if (hasUboSupport) {
	glUnmapBuffer(GL_UNIFORM_BUFFER);
	glBindBufferRange(GL_UNIFORM_BUFFER, UboBinding, buffer, offset, size)
} else {
	glUseProgram(program);
	glUniformMatrix4fv(locWorld, data.sliced(0, 64))
	glUniformMatrix4fv(locViewProj, data.sliced(64, 64))
}
```


[wiki-sprite-sheet]: https://en.wikipedia.org/wiki/Texture_atlas
[wiki-texture-compression]: https://en.wikipedia.org/wiki/Texture_compression
[wiki-mipmap]: https://en.wikipedia.org/wiki/Mipmap
[gh-toml]: https://github.com/toml-lang/toml
[about-opengl]: https://www.opengl.org/about/
[gl-uniform]: https://www.khronos.org/opengl/wiki/Uniform_(GLSL)
[gl-ubo]: https://www.khronos.org/opengl/wiki/Uniform_Buffer_Object
[gl-persistent]: https://www.khronos.org/opengl/wiki/Buffer_Object#Persistent_mapping
