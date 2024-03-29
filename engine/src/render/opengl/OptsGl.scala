package render.opengl

/**
  * Determines which OpenGL mechanism is used to write data to dynamic buffers
  * from the CPU. Some ways are faster than others, but require newer OpenGL
  * versions and others may have better compatability with old drivers.
  */
abstract class MapMode {
  def persistent: Boolean
  def coherent: Boolean
}

object MapMode {

  /**
    * Map the buffer contents with glMapBuffer and write the data directly.
    * This mode should have better performance with large buffers than SubData,
    * but may cause compatability issues with some GL implementations.
    *
    * As this engine requires GL >= 3.3 this map mode can use glFlushMappedRange()
    * with GL_MAP_FLUSH_EXPLICIT_BIT to flush only a portion of the mapping.
    *
    * buf = glMapBuffer(GL_BUFFER)
    * writeData(buf)
    * glUnmapBuffer(GL_BUFFER)
    */
  case object Map extends MapMode {
    def persistent = false
    def coherent = false
  }

  /**
    * Don't map the buffer directly, but write the contents to a temporary local
    * buffer and then transfer the data using glBufferSubData. This implementation
    * should have the best compatability, but probably at a cost of some potential
    * performance.
    *
    * buf = alloca()
    * writeData(buf)
    * glBufferSubData(GL_BUFFER, buf)
    */
  case object SubData extends MapMode {
    def persistent = false
    def coherent = false
  }

  /**
    * The "modern GL" way to map buffers, the buffer is initialized with immutable
    * storage, which means it will stay locked in place in memory as long as it
    * exists. Then the buffer needs to be mapped only once in the beginning with
    * the persistent bit. This way the CPU can write directly to the mapped buffer
    * and just flush the modified ranges.
    *
    * Persistent mapping is the fastest way to write data, but unfortunately it
    * requires GL >= 4.4. It can still be used if the `GL_ARB_buffer_storage`
    * extension is enabled.
    *
    * glBufferStorage(GL_BUFFER, GL_PERSISTENT_MAP_BIT)
    * persitentBuf = glMapBuffer(GL_BUFFER, GL_PERSISTENT_MAP_BIT)
    *
    * ...
    *
    * writeData(persistentBuf)
    * glFlushMappedRange(GL_BUFFER)
    */
  case object Persistent extends MapMode {
    def persistent = true
    def coherent = false
  }

  /**
    * Uses the same GL API as MapMode.Persistent, but instead of writing data
    * directly to the mapped memory it is written first to a local buffer and
    * then copied to the mapping with `memcpy()`. Use this if the Java memory
    * mapping doesn't play well with mapped GPU memory.
    *
    * buf = alloca()
    * writeData(buf)
    * MemoryUtil.memCopy(persistentBuf, buf)
    * glFlushMappedRange(GL_BUFFER)
    */
  case object PersistentCopy extends MapMode {
    def persistent = true
    def coherent = false
  }

  /**
    * Behaves like MapMode.Persistent, but instead of being manually flushed with
    * `glFlushMappedRange()` the buffer is created with `GL_COHERENT` bit
    * which guarantees that the memory writes are uncached.
    *
    * glBufferStorage(GL_BUFFER, GL_PERSISTENT_MAP_BIT|GL_COHERENT_BIT)
    * persitentBuf = glMapBuffer(GL_BUFFER, GL_PERSISTENT_MAP_BIT)
    *
    * ...
    *
    * writeData(persistentBuf)
    */
  case object PersistentCoherent extends MapMode {
    def persistent = true
    def coherent = true
  }

  /**
    * Behaves like MapMode.PersistentCopy, but instead of being manually
    * flushed `glFlushMappedRange()` the buffer is created with `GL_COHERENT`
    * bit which guarantees that the memory writes are uncached.
    *
    * glBufferStorage(GL_BUFFER, GL_PERSISTENT_MAP_BIT|GL_COHERENT_BIT)
    * persitentBuf = glMapBuffer(GL_BUFFER, GL_PERSISTENT_MAP_BIT)
    *
    * ...
    *
    * buf = alloca()
    * writeData(buf)
    * MemoryUtil.memCopy(persistentBuf, buf)
    */
  case object PersistentCopyCoherent extends MapMode {
    def persistent = true
    def coherent = true
  }
}

object OptsGl {
  /** How to map uniform block buffers */
  var uniformMap: MapMode = MapMode.PersistentCopy
  /** If `uniformMap` mode is not supported, fallback to this mode. */
  var uniformMapFallback: MapMode = MapMode.SubData
  /** How to map dynamic vertex buffers */
  var vertexMap: MapMode = MapMode.Persistent
  /** If `vertexMap` mode is not supported, fallback to this mode. */
  var vertexMapFallback: MapMode = MapMode.Map
  /** Should the implementation use uniform blocks or direct uniforms */
  var useUniformBlocks = true
  /** Should the implementation use only single VAO instead of caching */
  var useVaoCache = true
  /** Enable glTexStorage if available */
  var useTexStorage: Boolean = true
  /** Use the `std140` layout for UBOs without querying the layout */
  var useUboStd140 = false
  /** Use row major matrices instead of GL default column major */
  var useRowMajorMatrix = true
  /** Use OpenGL debugging features (KHR_debug available and requested) */
  var useDebug = false
  /** Use OpenGL profiling timers */
  var useProfiling = false
  /** How many vertical syncs to wait between swaps (0 for no vsync) */
  var swapInterval: Int = 1
}

