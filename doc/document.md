# Project document

## Description

This is a tower defence game where turrets can't aim themselves.
The goal is to guard some kind of crystal in the center of the level by
building turrets and radars, and then connecting them with cables.

![
Screenshot of the game.
Some kind of aliens approach turrets and radars.
](image/desc-01-screencap.png)&shy;

## Building

The source code for the project is located in repository [raivios1/tower][gl-tower]
and the content in [raivios1/tower-data][gl-tower-data]. Make sure you have
both checked out before starting to build the project.

This project is separated into multiple sub-projects located in the root
folder. The projects contain project files for IntelliJ IDEA and Eclipse.
The main startup class for the game is `main.EditorMain` in the project
`editor`, which supports building resources and starting the game simultaneously.

When running set the working directory to the root of your checkout of `tower-data`
and the program argument as: `-P config.toml`. If the program has issues with
graphics you can try running with `--debug` for diagnosing problems or `--gl-compat`
for using a more compatible OpenGL profile.

![
Eclipse "Run Configurations" window.
Program arguments: "-P config.toml"
Working directory selected as Other: D:\dev\test\tower-data
](image/build-01-eclipse-config.png)&shy;

## Code quality disclaimer

The engine has a ton of non-idiomatic and ugly Scala. This is all for one
major reason: to fight the garbage collector. Common wisdom in programming
is to avoid premature optimization. Sadly garbage collection overhead is not'
like having hot inner loops that you can diagnose and optimize. Everything
contributes a little bit, and at some point the game is stuttering.

For the best experience the game shouldn't allocate at all outside of loading
breaks, but that limits the programming style drastically in Scala, as it
doesn't have real value types. However if assuming that the JVM we're running
on has a generational garbage collector it's alright to generate _short lived garbage_.
For instance `Vector3` is represented as an immutable class and new ones are
created constantly, but since they're not retained for long the garbage collector
hit is reduced.

Another thing to note is using `while`-loops instead of `for`-loops. This is
not for GC reasons, but since Scala `for`-loops are extremely slow, as they're
transparently transformed into invocations of `foreach` with an abstract
callback argument. This means all the locals need to be lifted into a closure
and all the optimizations are off the table. Also in many places I use nullable
references instead of `Option`, since `Option` always requires and indirection
which adds both cache and GC overhead.

During the project I experimented with a ton of different approaches to making
Scala performant. In the oldest pieces of code I used to split data up to
structure-of-arrays style code such as (`IdentifierIx` is an alias for `Int`):

```scala
// Meshes
var meshParentNode: Array[Int] = Array[Int]()
var meshName: Array[IdentifierIx] = Array[IdentifierIx]()
var meshResource: Array[IdentifierIx] = Array[IdentifierIx]()
var meshMaterialIndex: Array[Int] = Array[Int]()
var meshes: Array[Mesh] = Array[Mesh]()
var numMeshes: Int = 0
```

This is the only way to get contiguous arrays on JVM. If the code would have
been better Scala it might have looked something like this:

```scala
case class MeshInfo(parentIndex: Int,
          name: Identifier,
          resource: Identifier,
          materialIndex: Int
          mesh: Mesh)

var meshes: Vector[MeshInfo] = Vector[MeshInfo]()
```

However here every `MeshInfo` instance is it's own reference somewhere in
memory, and the internal tree structure `Vector` causes even more GC overhead.
It's debatable if optimizing this actually has any benefit, since models generally
have a low amount of meshes anyway, but I was still getting adjusted. If I'd
do the class now I wouldn't bother with this optimization.

After getting fed up with dealing with the separate arrays I went one step
further. I made `Struct` which allows you to create contiguous array of
heterogenic data C-style. The usage looks like this:

```scala
object CharInfo extends Struct {
  /** Amount to advance per character */
  val Advance = float
  /** Offset into the kerning table */
  val KernOffset = int
  /** Number of entries in the kern table for this character */
  val KernCount = int
  /** Amount the character spans before the current position */
  val LeftSideBearing = float
}

...

val D = this.data
val CA = charInfoBase + index * CharInfo.size

val kernOffset = CharInfo.KernOffset.get(D,CA)
val kernCount = CharInfo.KernCount.get(D,CA)
```

Here the data is laid out manually in a `ByteBuffer` and accessed through
getters. Potentially extremely overkill, but this has very low GC overhead and
assuming JVM does the right thing should result in very good performance.
This approach is not used except in places that have hundreds of instances.
Both of these approaches are overkill for most features and the game code itself
barely uses anything like this. 

Still to be honest, most of the code quality issues can't be explained on such
noble merits. The scope of the project was just too large for the timespan and
I had to make sacrifices on the quality. There's abstractions half-baked just up
to the level sufficent for the game. There's parallel systems to do same things
as I got fed up with the old systems and made newer ways to do things. As such
the codebase is quite organic and possibly the youngest legacy codebase I have
worked with.

## Program structure

The projects in the game belong to a loose hierarchy depending on how re-usable
the code is. The project `shared` is just common utilities that could be used
by about anything. `resource` could be used as a resource processer for a
completely independent game. The `engine` project
starts to lay out some more restricting framework, but could still be used to
build lots of different kinds of 2D and 3D games. `game` contains a lot of game
specific things intermixed with more general utilities, which could be salvageable
with some work. The `test` project depends on everything and contains both very
game-specific parts and general ones. `editor` is simply a launcher that runs the
`game`, but bundling `resource` to processes attached, the idea was to have a
separate project for the final release which wouldn't be dependent on `resource`.

### shared

#### core

This package is intended to be imported everywhere and contains utilities that
are useful and necessary for making games. Most importantly it defines the
linear algebra classes such as `Vector3` and `Matrix43`. It also contains some
very useful data structures like `ArrayPool` or `CompactArrayPool`. The package
object defines some useful functions such as `clamp()` or `wrapAngle()`.

#### io

Base input/output functionality. Most important file here is `Toml.scala` which
implements an incomplete [TOML][gh-toml] parser used for all sorts of configuration
files in the engine. GThe parsed results are mapped to Scala classes using
`SimpleSerialization` which is also defined in this package.

#### util

Useful utilities that are not as fundamental as the things defined in `core`.
`SparseGrid` is a nice lightweight spatial partitioning data structure which
represents space as a hash-map of quantized position to grid cells, supporting
queries over points and ranges. `BinarySearch` implements `upperBound()` which
is an extremely useful building block for searching in abstract sorted ranges.
`AStar` implements a generic version of the [A\*-search algorithm][wiki-astar]
search algorithm.

#### util.geometry

Geometric primitives and intersections. The basic shapes are `Aabb`, `Sphere`,
and `Plane`. All the basic shapes support any vs. any intersection tests, eg.
`Aabb.intersects(Plane)`. Non-basic primitives `Ray` and `Frustum` can be used
to test intersections and hit distances on basic shapes, but for example frustum
vs frustum intersection is not supported.

### resource

#### cli

Contains the command line interface for running the resource processer.
Has been superseded by the `editor` project.

#### io

Input/output utilities needed by the resource processing. Contains `PathUtil`
which is used to recursively walk through the asset directories.

#### util

Algorithms useful for processing resources. `RectanglePacker` defines an interface
for rectangle packing algorithms used for font and sprite packing. Turned out that
[`LightmapRectanglePacker`][lightmap-rect] was good enough for both. `BufferHash`
is used for caching resource processing results by hashing asset input/output
files and comparing them to cached values.

#### res.importer

The first stage of processing an asset. Contains importers for different file
formats, some hand-written some using a library. The file `Importer.scala`
defines the interface all of the importers use and the mapping between names
and importer implementations. `ImportFileType.scala` is used to filter the
relevant configuration properties per asset so that changing an sound config
doesn't cause the re-processing of texture assets. It also contains processer
format versions, which can be incremented to force a re-processing of all assets
of a specific type.

#### res.intermediate

All the intermediate representations of the imported/processed assets. The most
important file here is `Config.scala` which contains the whole specification for
the asset configuration. Asset configuration `.ac.toml` files are parsed using
`util.Toml` and `util.SimpleSerialization` to these classes. The deserialized
configuration classes are then passed onwards to the relevant processes.

* AssimpImporter: Used for *.fbx* model files using [Assimp](http://www.assimp.org/)
* EntityImporter: Used for *.es.toml* entity specification files using `util.Toml`
* GlslImporter: Used for *.glsl* shader files, just reads the source file text
* LocaleImporter: Used for *.lc.toml* locale files using `util.Toml`
* StbImageImporter: Used for *.png* image files using [stb\_image](https://github.com/nothings/stb)
* StbTruetypeImporter: Used for *.ttf* font files using [stb\_truetype](https://github.com/nothings/stb)
* StbVorbisImporter: Used for *.ogg* audio files using [stb\_vorbis](https://github.com/nothings/stb)
* WavImporter: Used for *.wav* audio files using custom wave header parsing

#### res.process

The actual processing steps, they either operate in-place on some intermediate
asset or map it from one type to another. These attempt to be as-pure-as possible
functions so that they can be run in parallel for different assets to speed up
the processing time. Just reading the filenames and comments on the objects should
give some perspective on the transformations the resource processing applies to
the assets.

* AnimationCleanup: Flip quaternions so the interpolation distance is minimized
* BakeFont: Render truetype font glyphs into a bitmap
* CompressDxt: Convert textures to GPU-friendly format using [stb\_dxt](https://github.com/nothings/stb)
* CreateGpuMesh: Convert intermediate mesh into a GPU-friendly vertex stream
* CreateTexture: Convert intermediate image to GPU-friendly textures (compressed or not)
* CropSprite: Crop the transparent area around a sprite
* FlattenLocale: Transform a hierarchial TOML-based locale into a flat key-value list
* FlattenModel: Transform a hierarchial node-tree model into a flat list of nodes
* GenerateAtlas: Layout and draw sprites into a larger image
* GenerateMipmaps: Generate a [mipmap][wiki-mipmap] chain for an image
* HyphenateLocale: Add [soft hyphens][wiki-shy] to localization file strings
* JoinMesh: Join multiple separate meshes into one that can be rendered at once
* MeshCleanup: Utilities for cleaning up mesh bone influences on vertices
* OptimizeVertexCache: Re-order mesh vertices into a more GPU-friendly format using [Tom Forsyth's algorithm](https://tomforsyth1000.github.io/papers/fast_vert_cache_opt.html)
* PatchAnimationProperties: Patch animation configuration properties from a configuration file
* PatchModelProperties: Patch model configuration properties from a configuration file
* PcmProcess: Re-sample, flatten and encode [PCM][wiki-pcm] data
* PremultiplyAlpha: Pre-multiply the color channel with the alpha for better texture filtering and blending
* PreprocessShader: Apply custom GLSL shader pragmas `#pragma import` and `#pragma extension`
* ProcessAnimation: Apply lower-level animation processing based on configuration
* ProcessColorgrade: Crop the color-lookup image from the top-right corner of an image
* ProcessMesh: Apply lower-level mesh processing based on configuration 
* ProcessMeshBones: Algorithm for splitting mesh into chunks with a fixed maximum amount of bones
* ProcessSprite: Apply other sprite processes and split animation frames
* ReduceAnimationKeyframes: Optimize animations by reducing redundant keyframes
* ResolveMaterial: Match material texture names to real processed textures
* SaveDebugImage: Save a *.png* file of an image for debug purposes
* ScaleModel: Apply a scale to a model

#### res.output

Contains the actual output file serialization steps. Each file here should
correspond to one `.s2**` output file format with the exception of `MeshFile`
which writes both mesh `.s2ms` and mesh part `.s2mp` files. They are mostly
clean of any processing and should be a simple serialization from some
intermediate format. These files are the de-facto asset file format specifications.

#### res.runner

The engine which runs rest of the asset processing machinery. Contains definitions
of `AssetCacheFile` and `OutputCacheFile` which are to reduce unnecessary processing
and output writing respectively. These files are located in the */temp/*-folder with
extensions `.s2ac` and `.s2oc` and they contain hashes of the relevant files and
configurations for later comparison. `OutputFileWriter` is a wrapper on top of
normal file output which checks and maintains the output cache. The idea of the
output cache is not to wear out SSD:s by not writing files that haven't changed.
`RunOptions` contains the options used to launc hthe runner and contains the
deserialized version of the `config.toml` file in the asset repository root.

`Runner` is probably the best entrance to reading to the resource processing code.
It lists the assets to process, merges configurations, spawns worker threads, and
most importantly kicks off the import, processing, and output tasks.

### engine

#### asset

Defines different loadable/unloadable asset types. The main class here is
`LoadableAsset` which defines the lifecycle of an asset and its dependencies.
Assets are handles to content files that may or may not be loaded. Calling
`asset.get` will forcibly load the asset in the middle of everything and emits
a warning. The right way to use assets is to collect them in `AssetBundle`s and
reference them before entering a loading screen, which will display a loading
animation until all referenced assets are loaded. The assets are also capable
of being hot-loaded so that external changes can be propagated into the game
without restarting the program.

#### audio

The core audio functionality. `Sound` represents one audio asset, `SoundInstance`
is as the name implies an instance of a sound that has it's own playback
position and parameters such as volume or pitch. The sub-pacakge `source` contains
implementations for different sound file formats. `output` contains back-ends
for the audio output. `effect` defines only one effect `Limiter` which works as
a dynamic range compressor to keep the final output from cracking on unexpected
loud noises.

#### core

The engine doesn't add much to the `core` package except a wrapper for LWJGL's
native memory allocation routines so they can be used in packages that depend
on `engine` but not LWJGL itself.

#### debug

Implements a resource tracker that can check for leaks. For example if you
allocate an OpenGL vertex buffer and never free it, `ResourceTracker` will
complain when closing the game. The tracker captures a call-stack of frames
where resources are created so when reporting leaks it's very easy to diagnose
and fix these issues.

#### render

The low-level rendering API. Mostly a wrapper over OpenGL but should be possible
to port to other backend APIs with relatively little effort. The actual implementation
is under the sub-package `opengl`.

#### gfx

The high-level rendering API. Contains classes like `Model` and `Texture` that
can be loaded from asset files.

#### io.content

Contains `Package` which is the virtual file system abstraction used for loading
content. Supports loading content out of plain folders, content packed into the *.jar*,
and *.zip* files for mods and patches.

#### io.format

Contains limited implentations writing for TIFF *.tiff* and OpenEXR *.exr* file
formats. Used for exporting HDR image data for authoring tonemapping images.

#### io.property

The new property system superseding `SimpleSerialization`. The most important
functionality here is `MacroPropertySet` which is what makes the propeties usable
in the first place. You can use properties by defining class members as `*Prop.Type`
instead of plain types and then gathering the properties using a macro:

```scala
object PersistentState {
  private val arr = MacroPropertySet.make[PersistentState]()
  private val propertySet: PropertySet = new PropertySet("EnemySpawnSystem.PersistentState", arr)
}

class PersistentState extends PropertyContainer {
  override def propertySet: PropertySet = PersistentState.propertySet

  var gameSeed: IntProp.Type = 0
  var roundIndex: IntProp.Type = 0
}
```

#### io.serialization

Functionality for serializing/deserializing `PropertyContainer`s described above
into different binary formats. `BinaryWriter` and `BinaryReader` are used for
save files and `UnstructuredBinaryReader` is used for entity specification assets.

#### locale

Contains localization implementation, pretty much a glorified hash-map loaded
from a file. Supports expressions in locales such as `Hello {thing}` where
`{thing}` would be replaced with some other text from the code.

#### platform

Interfacing with the operating system. `AppWindow` manages the GLFW window and
collects events into more easily processed streams.

#### task

Contains a task-based multithreading implementation. Tasks can be assigned to
run on different threads (even on main thread) as needed. `Scheduler` implements
a way of automatically generating dependencies between tasks depending on what
data they read or write.

#### ui

Even though named UI for user interface this package contains all the 2D base
2D rendering functionality of the engine. The main component being `SpriteBatch`
which can render 2D images efficiently.

In addition to the 2D rendering API it contains higher-level `Canvas`, `InputSet`,
and `Layout` APIs that can be used for building user interfaces. These should
probably be split off into their own package (preferably by moving the 2D rendering
functionality somewhere else)

#### main

The "entrypoint" of the engine defining setup/teardown functionality.

### game

#### game.component

Contains classes for components that can be configured from entity specification
*.es.toml* files. Explained in more depth in the section entity-component-system.

#### game.lighting

Alternative light-probe implementations. Originally the game used `AmbientCube`,
but at some point transitioned to using second order spherical harmonics with
`SphericalHarmonics2`. However with real content the spherical harmonics ringing
artifacts were unbearable and now `AmbientCube` is used again.

#### game.menu

Some menus used in the game. Most importantly contains the sub-package `gui`
which defines a set of re-usable half-immediate half-retained mode GUI elements.

#### game.options

Contains deserialized options classes for user preferences.

#### game.shader

A collection of all the shaders and uniform blocks used by the game.

#### game.state

High-level game state implementations, such as main menu, loading screen, and
in-game.

#### game.system

Almost everything above is just utilities, all the actual game and engine logic
is contained in this package. See the section entity-component-system for more
information on the high-level structure of systems. The root `system` pacakge
contains the implementation for the vital `Entity` class which is used to bridge
all the systems together.

#### game.system.audio

* AudioSystem: Core audio playback: Playing sounds either by fire-and-forget or by controlled loops. Manages the background audio thread and `SoundInstance` parameter synchronization between threads
* SceneAudioSystem: Implements a 3D positional audio system modifying the volume and pan of sounds in space

#### game.system.gameplay

* BuildSystem: Manages the user interface and logic of player building towers
* BulletSystem: Bullet effects, should have been replaced with a general effect system
* CableSystem: Routes the cables between the towers
* ConnectionSystem: Handles the gameplay side of passing messages in the cables
* EnemySpawnSystem: Spawns rounds of enemies
* EnemySystem: Updates the spawned enemy instances and drives a very basic AI
* PathfindSystem: Used for enemy pathfinding
* PauseSystem: High-level build-mode/play-mode implementation
* SaveStateSystem: Handles persistent state about towers and cables
* TowerSystem: Contains implementation of _all_ the towers, should have been split more granularly
* TutorialSystem: Implentation of the tutorial

#### game.system.rendering

* AmbientPointLightSystem: Point lights that are applied to ambient light probes
* AmbientSystem: Ambient light probe manager
* AnimationSystem: Drives the skinned model animations
* CableRenderSystem: Generates the cable meshes
* CullingSystem: Handles frustum culling so only entities in the view need to be processed by certain systems
* DebrisSystem: Handles broken tower effect debris
* DirectionalLightSystem: Contains global sunlight parameters and shadowmap
* ForwardRenderingSystem: Generates draw-calls from mesh instances for the forward rendering pass
* GlobalRenderSystem: Manages global render targets
* GroundSystem: Generates the ground meshes and manages ground light probes
* ModelSystem: Updates model transformation matrices and collects instanced visible meshes
* PointLightSystem: Non-ambient point-light implementation (never used)
* ShadowRenderingSystem: Generates draw-calls for the shadow map rendering phase

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

## Entity-component-system

The game engine code is built using the [entity-component-system][wiki-ecs]
methodology. This has the benefit of systems that conform to the Unix philosophy:
Do one thing and do it well. By using separate systems no compromises need to
be made about data layout, which is useful for performance.

**Entity** by itself is just a handle, in itself it doesn't have any meaning.
In the purest ECS implementations the concept of entity can just be an integer
handle. For convenience I decided to allow some minimal data for entities:
A local transform represented by a position and a rotation and a set of 256
flags.

**Component** describes an aspect of an entity, such as having some model or
a bounding box of a specified size. The components are specified in entity
specification *.es.toml* files, which makes authoring different kinds of entities
easy.

**System** manages the internal behavior of the aspects of entities. When an
entity has some component it's added to the relevant system(s) that will manage
the entity from there.

### Example

To illustrate how these systems interplay let's go through a simplified example
of rendering a stone model. First we need to create the entity file, let's say
*stone.es.toml*:

```toml
# Systems may assume the entity never moves
static = true

# Component managed by CullingSystem
[BoundingAabb]
min.x = -10.0
min.y = 0
min.z = -10.0

max.x = 10.0
max.y = 10.0
max.z = 10.0

# The entity should be included when querying renderable
# and shadow casting objects
mask = "Render Shadow"

# Component manged by ModelSystem 
[Model]
asset = "stone.fbx.s2md"

# Make it a little flatter
scale.y = 0.7
```

The components are mapped to subclasses of `Component`, source below
has been simplified for clarity. The `mask` property of the bounding
box component will be converted from string to integer using a
context-dependent converter, otherwise the properties are mapped
one-to-one.

```scala
class BoundingAabbComponent extends Component {
  var mask: IntProp.Type = 0
  var min: Vector3Prop.Type = Vector3.Zero
  var max: Vector3Prop.Type = Vector3.Zero

  override def create(entity: Entity): Unit = {
    cullingSystem.addAabb(entity, Aabb.fromMinMax(min, max), mask)
  }
}

class ModelComponent extends Component {
  var asset: IdentifierProp.Type = Identifier.Empty
  var scale: Vector3Prop.Type = Vector3.One

  override def create(entity: Entity): Unit = {
    val model = modelSystem.addModel(entity, asset)
    model.scale = scale
  }
}
```

To instantiate the entity we need to load the `EntityType` which contains a list
of the components that make it up as well as whether the entity is static and such.
We can load the stone using an `EntityTypeAsset` which has the benefit of handling
dependencies loading the stone model for us.

```scala
// Define the asset, but don't load it yet
val StoneTypeAsset = EntityTypeAsset("stone.es.toml")

// Getting the result from the asset loads the entity type and the
// stone model here. In a real game it should be loaded asynchronously.
val stoneType = StoneTypeAsset.get

// Instantiate the entity using EntitySystem
val position = Vector3(10.0, 0.0, 5.0)
val stone = entitySystem.create(stoneType, position)
```

The `EntitySystem.create()` method is actually extremely simple: It just calls
`create()` on all the components that belong to the entity type. A cavaeat here
is that the components may have dependencies between them. This is solved by
pre-sorting the components to dependency order when importing the entity types.

```scala
def create(entityType: EntityType, position: Vector3, rotation: Quaternion): Entity = {
  val entity = new Entity(entityType.static, entityType.name, entityType)
  entity.position = position
  entity.rotation = rotation
  for (comp <- entityType.components) {
    comp.create(entity)
  }
  entity
}
```

As defined above the `create()` functions for the two components that make up the
stone are both quite simple: one calls `cullingSystem.addAabb()`, the other
`modelSystem.addModel()`. The bounding box creation is quite involved and doesn't
really matter for this example, but feel free to skim over the source quickly to get
some feel of how the systems look inside:

```scala
override def addAabb(entity: Entity, aabb: Aabb, mask: Int): Int = {

  // This line adds the entity to the culling system and
  // creates an internal representation for the entity.
  val cullable = getOrAddCullable(entity)

  // If the entity is rotated the bounding box needs to be adjusted
  val rotatedAabb = if (entity.rotation != Quaternion.Identity) {
    aabb.rotate(entity.rotation)
  } else {
    aabb
  }

  // Create the shape used for culling
  cullable.aabb = new ShapeAabb(cullable, rotatedAabb, null, mask,
                                cullable.aabb, nextSerial())

  // If the entity is _not dynamic_ add the shape directly
  // to the quad-tree data structure. Dynamic objects are
  // added separately each frame.
  if (cullable.dynamicIndex < 0)
    addShape(cullable.aabb)

  // Return a handle to the shape so it can be removed
  cullable.aabb.serial
}
```

The model system addition is a little bit clearer. Remember how the entities
had a transform and some flags: Note that the model system tags the entity with
`Flag_Model`, which will be important later.

```scala
override def addModel(entity: Entity, asset: ModelAsset): ModelInstance = {

  // Common style inside systems: Internal object have an
  // embedded linked list to support multiple models per
  // entity while not adding overhead to the general case
  // of maximum one model per entity.
  val next = if (entity.hasFlag(Flag_Model))
    entityToModel(entity)
  else
    null

  // Create the internal model representation
  val model = new ModelInstanceImpl(entity, asset, next)

  // Insert the model into two data structures: the list
  // of all models and the map from entities to models
  model.poolIndex = allModels.add(model)
  entityToModel(entity) = model

  // IMPORTANT: Set the `Flag_Model` flag for the entity
  entity.setFlag(Flag_Model)

  // Return the created model instance, note how the return
  // type is public `ModelInstance` while the actual type
  // of it is `ModelInstanceImpl`
  model
}
```

So now the stone is in two separate systems which don't seem to know about each
other. This is the common pitfall in entity-component-system style architectures,
but I think the flag approach solves the bridging of systems quite nicely.
When rendering the engine will query `CullingSystem` about which entities are
currently visible in the viewport.

```scala
/**
  * Find all entities with bounding areas tagged with `mask` which intersect
  * with `frustum`. The results are gathered to `set`.
  */
def cullEntities(set: EntitySet, frustum: Frustum, mask: Int): Unit
```

The implementation of the culling itself is quite complicated and involves
walking some quad-trees and performing intersection tests. The notable part
here is the collection the entities are gathered to: `EntitySet`. Unlike a
generic collection `EntitySet` supports partitioning the entities into flag-specific
subsets efficiently. As the flags are stored in bit-masks we can do super fast
bit-operations to assign the entities to the correct buckets. By using this bucketed
approach we avoid the case of every system needing to check every visible entity
if it belongs to the system.

```scala
/**
  * A set of entities. Does not internally check that entities are unique, but
  * contractually only contains unique entities.
  */
class EntitySet {

  /** Every entity in the set */
  val all = new ArrayBuffer[Entity]

  /** Entities which contain a specific flag */
  val flag = Array.fill(256)(new ArrayBuffer[Entity])

  /** Add an entity to a flag-specific list.
    *
    * Performance is relative to the number of set flags per entity,
    * unused flags are practically free.
    *
    * @param entity Entity to add
    * @param mask Flag mask bits
    * @param base Index of the first mask bit
    */
  private def addByFlag(entity: Entity, mask: Long, base: Int): Unit = {
    var maskBits = mask

    // Is there still set flags in this group?
    while (maskBits != 0) {

      // Use the CTZ instruction to calculate the index
      // of the lowest bit
      val index = java.lang.Long.numberOfTrailingZeros(maskBits)

      // Clear the lowest bit and add the entity to the sub-set
      maskBits &= (maskBits - 1)
      flag(base + index) += entity
    }
  }

  /** Add an entity to the set.
    * Note: Never add an entity twice to a set. */
  def add(entity: Entity): Unit = {
    all += entity
    addByFlag(entity, entity.flag0, 0)
    addByFlag(entity, entity.flag1, 64)
    addByFlag(entity, entity.flag2, 128)
    addByFlag(entity, entity.flag3, 192)
  }
}
```

Now the `CullingSystem` has done it's job and collected all the currently visible
entities. The visible `EntitySet` is passed on to `ModelSystem.collectVisibleModels()`
which gathers the internal model instances from a set of entities. Note how the
method doesn't need to check whether the entities have a model or not, since it
goes through the sub-set of entities with the flag `Flag_Model`.

```scala
override def collectVisibleModels(visible: EntitySet): ArrayBuffer[ModelInstance] = {
  val result = new ArrayBuffer[ModelInstance]()

  // Go through each visible model with `Flag_Model`
  for (entity <- visible.flag(Flag_Model)) {

    // Get the first internal model instance for the entity
    // and add it to the result
    var model = entityToModel(entity)
    do {
      result += model

      // Loop through the embedded linked list in case the
      // entity contains more than one model
      model = model.next
    } while (model != null)

  }

  result
}
```

Now all that needs to be done is to update and render the returned model instances.
When we're done with the stone we can call `stone.delete()`. The deletion also
uses an `EntitySet` for deleted entities. A set of deleted entities is given to
every system once every frame, then the systems can use the same bucketing mechanism
to remove the internal representation of entities that they are interested in.

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

### Buffer mappings

As explained above, the engine uses uniform buffer objects where available.
This raises the question: How do you transfer the UBO data to the GPU?
As is with most of OpenGL there is no clear solution.
Different styles are faster on different hardware and drivers, some things
can be flat out broken. So to deal with this there are lots of
ways to map buffers in the engine. Not only for uniform buffers, but for
example dynamic vertex data needs to be transferred somehow.

The two cases of mapping (uniform buffer objects and dynamic vertex buffers) have different
write patterns which affects the type of mapping we want. Uniforms tend to be
written out of order to the buffer and may even leave gaps. In contrast, vertex
data tends to be streamed out in a very linear fashion. The reason this matters
is that if the driver hands out a mapping to the GPU memory, it will most likely
be [write combined][wiki-wc]. Write combined memory doesn't behave well with
[erratic write patterns][ryg-wc]: While a perfect match for vertex data it
is not suitable uniform buffers. The solution the engine takes is that uniform
data is first written to a CPU local buffer and then copied to GPU memory in one
piece.

![
Writing uniform buffers with glSubData().
Mapping vertex buffers with glMapBuffer and glUnmapBuffer().
](image/map-01-old.png)&shy;

When working with older drivers the engine uses `glBufferSubData()` for uniform buffers.
The `glBufferSubData()` call copies data to a GPU buffer in an unspecified way,
which isn't ideal but it's better than doing tons of tiny mappings for every UBO.
Vertex data uses the `glMapBuffer()` API with `GL_UNSYNCHRONIZED_BIT`, which
should give the application a chunk of write combined GPU memory. The drawback is
that this can (ironically) cause synchronization stalls especially in multithreaded drivers.

![
Copying local buffer to uniform persistent mapping.
Writing data directly to vertex persistent mapping.
The mappings are optionally flushed with glFlushMappedBufferRange()
](image/map-02-new.png)&shy;

While the game only requires OpenGL 3.3, if the presence of the [`GL_ARB_buffer_storage`][gl-arb-buffer-storage]
extension is detected the engine uses persistent mapping. As the name implies
persistent mapping lets the application map the buffer into memory _once_ and
write to it even while the driver is using it. As with the legacy case uniform
buffers are not written directly to the GPU, but this time manually copied through
a local buffer. Vertex data is written directly to the GPU without almost any
API overhead from OpenGL.

OpenGL still needs some way to know when to flush the data from the mapping to
the GPU. To do this there are two options: Explicit flushing and coherent buffers.
When the buffers are created and mapped with explicit flushing enabled the
application needs to call `glFlushMappedBufferRange()` after writing to guarantee
that the data is visible to the GPU. If the buffer is coherent, this is not
required and the data is automatically guaranteed to reach the GPU. There is
no clear winner here: the performance is dependent on the platform.

There is one exception to the mapping of buffers: If OpenGL compatability mode
is requested, for example with `--gl-compat`, __everything__ is mapped with
`glBufferSubData()`, since it's the most foolproof API.

## Cable rendering

To get a feeling of the code flow of the engine let's go through an overview
of how the cables in the game get layouted and rendered.

The cable paths are represented as [cubic Hermite splines][wiki-hermite-spline],
which are defined as control points and tangents that the curve will pass through
and interpolate between. They are implemented in the utility object `core.Hermite`:

```scala
def interpolate(p0: Vector3, m0: Vector3, p1: Vector3, m1: Vector3, t: Double): Vector3 = {
  val t2 = t*t
  val t3 = t2*t
  val h00 = 2*t3 - 3*t2 + 1
  val h10 = t3 - 2*t2 + t
  val h01 = -2*t3 + 3*t2
  val h11 = t3 - t2
  p0*h00 + m0*h10 + p1*h01 + m1*h11
}
```

The cable paths are authored by hand near the towers. Control points are represented
as empty Blender objects parented under a main `Cables` object. The nodes are not
in a hierarchy themselves since that makes it hard to operate on them in Blender,
so they are hackishly organized by a complicated naming scheme.

![
Blender with turret model and cable control arrows pointing around.
](image/cable-01-blender.png)&shy;

Since the cable node objects are only used for static path generation, it would
be wasteful to keep them as normal nodes in the engine. To fix this they are
marked as **auxilary** in the `game/tower/models.ac.toml` asset configuration file:

```toml
# Cables: static cable control points
[[res.model.nodes]]
name = "Cables"
auxilary = true
```

This configuration is parsed into a configuration class using the built-in
TOML parser and `SimpleSerializable` traits:

```scala
class Node extends SimpleSerializable {

  /** Filter for node name */
  var name: String = ""

  /** Disables automatic world transform calculation */
  var auxilary: Boolean = false

  override def visit(v: SimpleVisitor): Unit = {
    name = v.field("name", name)
    auxilary = v.field("auxilary", auxilary)
  }
}
```

The `FlattenModel` flattens the hierarchy of nodes into a flat order where children
always follow their parents. The engine only supports top-level auxilary nodes and
they are ordered to be the last nodes in the linearized list. In runtime the engine
processes nodes only until the first auxilary node and the auxilary nodes in the end
of the list can be forgotten for most purposes. The cable paths are extracted by
`CableRenderSystemImpl.createCablePathsForModel()` which just matches the node names
using regex.

![
Short cable connecting a radar and a turret.
](image/cable-02-short.png)&shy;

If two towers are right next to each other these are mostly all the cable control
points that are needed. However, if there is a larger distance between the towers
the cable needs to be somehow routed in a way that it can dodge other towers.
There was some failed approaches to this, but the solution which worked takes
advantage of the grid-based nature of the tower positions. The actual pathfinding
work is implemented with the generic [A\*-search algorithm][wiki-astar] using `util.AStar`.

![
Long cable aligned in a grid.
](image/cable-03-grid.png)&shy;

Creating control points from the search path results in a cable that succesfully
dodges obstacles but visually bound to a grid. To solve this cable nodes are
iteratively removed if the resulting cable doesn't intersect with any obstacles.
The cable routing code is contained in `CableSystem`.

![
Long diagonal cable.
](image/cable-04-cleaned.png)&shy;

![
Long half diagonal cable with a grid part dodging obstacles.
](image/cable-05-obstacle.png)&shy;

Next, the control points need to be turned into a mesh. This is split into two
phases: First the spline is evaluated at points and then the points are converted
into thicker rings which make up a cable mesh. The Hermite spline is parameterized
by a single value that doesn't have any geometric meaning about the distance on the
curve. This means the spline can't just be evaluated at even offsets to get good results.
All the code for the cable mesh generation is located in `CableRenderSystem`.

The algorithm to evaluate the spline works as follows: Define minimum and maximum
increments in the spline parameter domain and an evaluation function whether an
evaluated point is meaningfully different from the previous one. The evaluation
function has minimum and maximum distance limits to the previous point and
maximum angle differecne based on the numerical derivative of the spline.

```scala
/** Returns whether this advance is of an acceptable distance and/or curvature */
def isConsiderableAdvance(t: Double): Boolean = {
  val nextPos = evaluate(t)
  val delta = nextPos - position
  val sq = delta dot delta
  if (sq <= minDistanceSq) return false
  if (sq >= maxDistanceSq) return true

  val derivPos = evaluate(t + MinTimestep)
  val derivTangent = derivPos - nextPos
  val length = derivTangent.length
  if (derivTangent.length > 0.0001) {
    val dot = derivTangent.normalize dot tangent
    if (dot >= minAngleDot) return false
  }
  true
}
```

After these definitions the rest of the spline evaluation algorithm is quite
simple. Advance the spline by the maximum argument increment until the next
point is acceptable.  In small enough intervals the evaluation function can
be considered monotonic, so we can solve the optimal point to add by binary
searching the interval between the previous unacceptable time and the next
acceptable one. After finding the optimal point it is added to the result
list and used as a reference previous point. The algorithm continues until
the end has been reached.

```scala
val positions = ArrayBuffer[Vector3]()
while (time < TimeEnd) {
  positions += position

  // Skip the cable one MaxTimestep at a time
  while (time < TimeEnd && !isConsiderableAdvance(time + MaxTimestep)) {
    time += MaxTimestep
  }

  // Find the optimal insert position
  val baseTime = time + MinTimestep
  val ix = BinarySearch.upperBound(0, BinarySeachGranularity, ix => {
    isConsiderableAdvance(baseTime + ix * binarySearchStep)
  })

  time = baseTime + ix * binarySearchStep
  val nextPos = evaluate(time)

  val deltaPos = nextPos - position
  val deltaLen = deltaPos.length
  if (deltaLen >= 0.0001) {
    tangent = deltaPos / deltaLen
  }
  position = nextPos
}
```

![
Hard case of S-shaped cable with 90 degree angles.
](image/cable-06-orientation.png)&shy;

To make the actual mesh, the list of points is iterated and converted into
rings. Cross products are used to keep the rings in the same orientation so
the cable doesn't twist when turning. In the picture above the green arrow
represents the ring normal, which can be obtained from the normalized direction
between the previous and next points. The red arrow is the tangent direction which
should stay consistent between the vertices. It turns out that it's easier to
first solve the bitangent for a ring and derive the tangent from that (by simple
cross product between the normal and bitangent). The bitangent for a point can
be formed by a cross product of the _previous_ tangent and _current_ normal
vectors.

```scala
// Middle section
var pointIx = 1
while (pointIx < points.length - 1) {
  val p0 = points(pointIx - 1)
  val p1 = points(pointIx)
  val p2 = points(pointIx + 1)

  normal = p2 - p0
  bitangent = (tangent cross normal).normalize
  tangent = (normal cross bitangent).normalize
  appendRing(p1)

  pointIx += 1
}
```

The rendering implementation adds some constraints to the cable meshes. The most
limiting factor of cable mesh size is _light probes_. To receive ambient light
the cable mesh re-uses the light probes of the ground. When adding a ring to the
cable, `GroundSystem.getProbesAndWeights()` is called to determine the a weighted
average of the ground light probes for the midpoint. Since there is a maximum amount
of light probes that can be attached to a single draw-call, the mesh may have to
be split at points.

The resulting mesh parts are added to `CullingSystem` and tagged with `Flag_CablePart`.
When rendering the engine gathers all visible entities and at some point `CableRenderSystem`
goes through all visible objects with `Flag_CablePart` set and adds them into a list,
which later gets rendered using `CableShader`.

```scala
object CableShader extends ShaderAsset("shader/mesh/cable") {

  uniform(GlobalSceneUniform)
  uniform(LightProbeUniform)

  override object Textures extends SamplerBlock {
    val ShadowMap = sampler2D("ShadowMap", Sampler.ClampBilinearNoMip)
  }

  override object Defines extends Shader.Defines {
    both("MaxLightProbes", LightProbeUniform.MaxProbes)
    both("ShaderQuality", Options.current.graphics.quality.shaderQuality)
  }

}
```

## Testing

When building the game many types of testing were applied.

In games unit testing can be applied in a limited fashion as games contain lots
of aspects that cannot be automatically tested. Unit tests were used mainly for
core utilities, such as math primitives, some algorithms, and serialization
functions. This turned out to be exceptionally helpful on matrices and quaternions
as it forced me to evaluate and ensure the correct handedness of operations. For
example `Quaternion.fromAxisAngle(Vector3.Up, 1.0)` should give the same result as
`Matrix43.rotateY(1.0)` by definition.

During development manual integration test cases were created to test the features
without having to commit to building a full-sized game yet. The first one `TestScene` started
as an animated model walking, then background music was added, then text rendering
and some 2D graphics. The second major one `TestModelSystem` was built when creating
the system-based architecture of the engine. Finally `TestCableSystem` was used to
create the physically-based rendering shaders and first revisions of the cable
layout algorithm. These tests won't run anymore with the data provided in the
asset repository and require a specialized non-public test-data package.

To ensure that the game works on multiple platforms, compatability testing needed to be
done. Mainly everything worked acceptably between the systems, but the differing OpenGL
driver implementations revealed shader compiler errors that were ignored on other systems.
There were also a few OpenGL driver bugs that had to be worked around on some systems.
Systems the game has been tested on:

* Windows 10 desktop with NVIDIA GPU: Development machine, everything worked as intended
* Windows 10 laptop with Intel GPU: Worked OK on low settings
* Windows 8 laptop with Intel GPU: Worked OK on low settings
* Windows 8 laptop with NVIDIA GPU: Worked OK on medium settings
* Windows 8 laptop with ATI GPU: Issues with vertex attribute format packing (fixed)
* macOS laptop with AMD GPU: Issues with row-major matrices and retina cursor mapping (fixed)
* Linux laptop with Intel GPU: Issues with MSAA
* Linux desktop with NVIDIA GPU: Issues with MSAA

In addition to testing the functionality of the game focus testing has been employed
to test the gameplay and user interface. The cable connecting UI was changed from
window-based to in-game overlay to make it more understandable for players. The
tutorial has been extensively tested by many people and it seems to serve it's
purpose quite well.

## Post-mortem

In this section I evaluate the technical choices I have made during the project
and look at what worked and what could have been done differently. The topics
listed will be in roughly chronological implementation order.

#### Multi-project setup

Right at the start I wanted to separate the project into sub-projects that could
be build independently. This differs from just doing sub-packages by giving more
control over name visibility (eg. `engine` project has absolutely no way to
access anything from `game`). The project structure also allows the containment of external
libraries with the most important case being the asset importing libraries used by
the `resource` project. As the final game project should have no dependency on
`resource` so the (usually big or awkwardly licensed) asset libraries can be
omitted from the final distribution.

At the start I decided _not_ to do a top-level package for each project to make
it easier to refactor components from project to project. In the end this probably
was not the best idea and the final real project `game` got its own top-level
package.

#### TOML

When starting to build the prototype asset processing pipeline it turned out
that configuration files are a necessity. The options for the file format were
[JSON][json], [YAML][wiki-yaml], [TOML][gh-toml], and some custom format. I discarded
JSON first as it doesn't have comments and has an unfortunate amount of line-noise
for simple configuration purposes. YAML on the other hand looks pretty nice but
is overly complicated. TOML is a very minimalistic .ini-inspired _config file format_,
which seemed like a nice match. TOML still lacked some features which almost caused
me to develop a custom domain-specific configuration format, but the gains from it
were outweighed by the technical complexity of having a custom language in system.
The project contains a very minimalistic TOML-subset parser initially meant for
testing the asset system, but it has worked well enough that it never needed to be
replaced.

#### SimpleSerialization

Now that we have the ability to read configuration files it would be nice to be
able to deserialize them into some kind of structures in the code. I dabbled for
a while with macros and Java-based reflection but didn't like how magical those
solutions turned out to be so I made the simplest thing possible: A visitor that
manually goes through every field. This works alright but caused some bugs to
appear every now and then by forgetting to update the `visit()` method. Still
these bugs were minor enough and quick to diagnose so the system stood the test
of time.

For example here's an abridged source snippet from the top-level asset config
structure:
```scala
/** Root of the asset configuration file */
class Config extends SimpleSerializable {
  var filter = new ArrayBuffer[Filter]()
  var importer = new Importer()
  var res = new Res()
  var priority = 0.0

  override def visit(v: SimpleVisitor): Unit = {
    filter = v.field("filter", filter, new Filter)
    importer = v.field("importer", importer)
    res = v.field("res", res)
    priority = v.field("priority", priority)
  }
}
```

The `SimpleSerialization` also worked for binary serialization, which is used
to generate hashes of the configuration files for asset processing caching.

#### Asset processing

The asset processing is based on importers which import asset files into an
intermediate representation, processes which operate on said intermediate objects,
and output formats which serialize the asset to an engine-readable form. This
structure was flexible enough to support most of what needed to be done. Still
some features that I wanted to do were blocked by this structure. Sharing meshes
between different models didn't really work out. Pre-processing shaders imports
on process-time didn't work because of the dependency system. [Fixing texture UV seams][sylvan-uvopt]
would have been hard to fit as models and textures are independent, in addition to
the algorithm being a ton of work. On the positive side tacking on multithreading
onto the resource processer was trivial.

#### OpenGL wrapper

I'm very happy of the abstraction that built around OpenGL in the engine. It
grew quite organically around the current needs of whatever I was doing, but
it still turned out pretty flexible. The shader specification heavily uses Scala's
`object`-singletons to define blocks of information. The trick that made the
vertex specification API nice in my opinion is the lack of having to manually
keep track of strides and offsets. This works by the engine contiguously layouting
the vertex elements by their size. To support gaps you can create attributes with
type `PAD`, but this was never necessary in actual use-cases.

```scala
object UpsampleMsaaShader extends ShaderAsset("shader/msaa_upsample") {

  uniform(VertexUniform)
  object VertexUniform extends UniformBlock("VertexUniform") {
    val TextureScale = vec4("TextureScale")
  }

  override object Permutations extends Shader.Permutations {
    val SampleCount = frag("SampleCount", Array(2, 4, 8, 16))
  }

  override object Textures extends SamplerBlock {
    val Multisample = sampler2DMS("Multisample")
    val SubsampleWeights = sampler2DArray("SubsampleWeights", Sampler.ClampBilinearNoMip)
  }
}

val GroundSpec = VertexSpec(Vector(
  Attrib(3, F32, Identifier("Position")),
  Attrib(3, F32, Identifier("Normal")),
  Attrib(4, UI8, Identifier("ProbeIndex")),
  Attrib(4, UN8, Identifier("ProbeWeight")),
))
```

#### UI rendering

When making the font implementation I had one goal: Readable subpixel small fonts.
I spent a lot of time with `stb_truetype`s [font oversampling][gh-stb-oversample].
Sadly in the end after I got it working artifact-free it looked worse than simple
SDF implementation with large textures. The oversample-codepath still exists, but
is not used by any font. Otherwise the font rendering turned out fine, and using
uniform buffers it's able to squeeze a lot of text to different draw-calls.

To reduce the amount of draw-calls used by the 2D rendering, I made the observation
that the way I generate the atlases makes some amount of full-size pages and one
left-over texture page. Instead of creating a new draw-call every time a sprite is
rendered from another page I joined the first pages into one array texture and keep
the last one as separate. When rendering the sprites there is a  branch on whether
to use the array texture or the final one. This goes against the wisdom of no
branching in shaders, but it should be fine as the branch is very uniform.

#### Word wrapping

When I decided to add word-wrapping support to the text rendering I got the idea
of adding [soft hyphens][wiki-shy] to the localized text at processing time. First
idea was to use a dictionary for each language. This worked for English but after
some searching I couldn't find a Finnish one, so I adapted the hyphenation algorithm
from [`vepasto/finnish-hyphenator`][gh-finnish-hyphenator]. The first implementation
hyphenated at every chance which made the text a little hard to read, so I added a
tweakable threshold controlling how much empty space created if the word would be
wrapped as a whole.

#### Shading system

This is probably my biggest gripe with the engine. From the first render output
I got it became apparent that this game seriously needs antialiasing with the
small but polygonal towers. I decided to use [MSAA][wiki-msaa], since it's easy to
implement while resulting in good quality. Another option would be to use some
post-process antialiasing like [FXAA][wiki-fxaa], but I don't like the blurring it introduces.
Temporal antialiasing could have worked out but I didn't want to implement it for
this project. MSAA has one big drawback: it is very memory intensive for the render
targets, which means [deferred shading][wiki-deferred] is not really an option, since it requires
very fat G-buffers as well. Deferred texturing could have worked as an alternative,
but it requires the engine to be built around it. I would have wanted to use [clustered shading][pdf-clustered],
but since I wanted the game to require only OpenGL 3.3 it would have been very impractical
to implement without compute shaders. This left me with forward shading, which is
not a good solution to the combinatoric problem of lights and objects.

#### Systems and scheduler

This is another thing in the engine that I'm pretty happy about. The entities
of the scene is split into systems. The systems can be updated on multiple
threads using the `Scheduler` API and specifying the dependencies between different
update phases. This worked wonderfully and could be extended further with adding
possibilities to splitting updates into overlapping tasks and creating some sort
of mutual exclusion dependency, for when tasks `A` and `B` may not run at the same time,
but `A -> B` and `B -> A` are both valid.

#### Tonemapping

Using physically based linear high-dynamic-range light values in the renderer means that the resulting image
doesn't fit well for monitors to display. This means we need to apply a [tonemapping][wiki-tonemap]
function to the image to compress it for the low-dynamic-range display. First, I tried to implement
John Hable's [Filmic Tonemapping with Piecewise Power Curves][fw-piecewise-tonemap], but I wasn't
satisfied with how it was turning out. Then I decided to go with [the lookup table approach][witness-colorgrade]
in which you can take a snapshot from the game with an identity color matrix, manipulate
that image in any software of your choosing, then extract the color transformation from the
now transformed color matrix.

I wanted the exported reference image to have more than 8 bits per channel for HDR
data. This means that the simplest export format [bmp][wiki-bmp] is off the table.
I wanted to find a high bitdepth file format that [Krita][krita] supports, which
wouldn't require any compression on the exporting end (so 16-bit [png][wiki-png]
are out). First I implemented a [TIFF][wiki-tiff] exporter which seemed to work
fine and was good enough to author the main menu colorgrade, but after adding
textures to the game it turned out something was off: Krita didn't support TIFF's
[TransferFunction][tiff-transferfunction], which meant that I couldn't represent
HDR data without serious banding. To fix this I implemented a minimal [OpenEXR][wiki-openexr]
exporter which is nice (apart from the absolutely baffling file format) since it
supports floating point image data.

In the end the lookup method to tonemapping works fine even for HDR to LDR transform
when using a lookup table that covers a fixed maximum exposure and has sample points
distributed in a square curve. A nice detail of the asset processing sytem is that you can
save the modified screenshots as-is (instead of having to crop the color matrix)
which gives some context to the raw assets.

[wiki-sprite-sheet]: https://en.wikipedia.org/wiki/Texture_atlas
[wiki-texture-compression]: https://en.wikipedia.org/wiki/Texture_compression
[wiki-mipmap]: https://en.wikipedia.org/wiki/Mipmap
[wiki-wc]: https://en.wikipedia.org/wiki/Write_combining
[wiki-yaml]: https://en.wikipedia.org/wiki/YAML
[wiki-shy]: https://en.wikipedia.org/wiki/Soft_hyphen
[wiki-msaa]: https://en.wikipedia.org/wiki/Multisample_anti-aliasing
[wiki-fxaa]: https://en.wikipedia.org/wiki/Fast_approximate_anti-aliasing
[wiki-deferred]: https://en.wikipedia.org/wiki/Deferred_shading
[wiki-tonemap]: https://en.wikipedia.org/wiki/Tone_mapping
[wiki-bmp]: https://en.wikipedia.org/wiki/BMP_file_format
[wiki-tiff]: https://en.wikipedia.org/wiki/TIFF
[wiki-png]: https://en.wikipedia.org/wiki/Portable_Network_Graphics
[wiki-openexr]: https://en.wikipedia.org/wiki/OpenEXR
[wiki-hermite-spline]: https://en.wikipedia.org/wiki/Cubic_Hermite_spline
[wiki-astar]: https://en.wikipedia.org/wiki/A*_search_algorithm
[wiki-pcm]: https://en.wikipedia.org/wiki/Pulse-code_modulation
[wiki-ecs]: https://en.wikipedia.org/wiki/Entity%E2%80%93component%E2%80%93system
[gh-toml]: https://github.com/toml-lang/toml
[gh-stb-oversample]: https://github.com/nothings/stb/tree/master/tests/oversample
[gh-finnish-hyphenator]: https://github.com/vepasto/finnish-hyphenator
[about-opengl]: https://www.opengl.org/about/
[gl-uniform]: https://www.khronos.org/opengl/wiki/Uniform_(GLSL)
[gl-ubo]: https://www.khronos.org/opengl/wiki/Uniform_Buffer_Object
[gl-persistent]: https://www.khronos.org/opengl/wiki/Buffer_Object#Persistent_mapping
[gl-arb-buffer-storage]: https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_buffer_storage.txt
[ryg-wc]: https://fgiesen.wordpress.com/2013/01/29/write-combining-is-not-your-friend/
[json]: http://json.org/
[sylvan-uvopt]: https://www.sebastiansylvan.com/post/LeastSquaresTextureSeams/
[pdf-clustered]: http://www.cse.chalmers.se/~uffe/clustered_shading_preprint.pdf
[fw-piecewise-tonemap]: http://filmicworlds.com/blog/filmic-tonemapping-with-piecewise-power-curves/
[witness-colorgrade]: http://the-witness.net/news/2012/08/fun-with-in-engine-color-grading/
[krita]: https://krita.org/en/
[tiff-transferfunction]: https://www.awaresystems.be/imaging/tiff/tifftags/transferfunction.html
[gl-tower]: https://version.aalto.fi/gitlab/raivios1/tower
[gl-tower-data]: https://version.aalto.fi/gitlab/raivios1/tower-data
[lightmap-rect]: http://blackpawn.com/texts/lightmaps/
