package res.runner

import java.io.File
import java.nio.ByteBuffer
import java.nio.file.Paths

import util.BufferUtils._
import util.{BufferHash, BufferIntegrityException}

class OutputFileWriter(val opts: RunOptions) {

  val absoluteDataPath = new File(opts.dataRoot).getCanonicalFile.getAbsolutePath
  def dataRelative(file: File): String = {
    val absolute = file.getCanonicalFile.getAbsolutePath
    absolute.drop(absoluteDataPath.length + 1).replace('\\', '/')
  }

  /** Get the cache file associated for this output file */
  def getCacheFilePath(file: File): File = {
    val relPath = dataRelative(file)
    Paths.get(opts.tempRoot, relPath + ".s2oc").toFile
  }

  /** Try to read the output cache file */
  def readCacheFile(cacheFile: File): Option[OutputCacheFile] = {
    if (cacheFile.exists && cacheFile.isFile && cacheFile.canRead) {
      val cache = new OutputCacheFile()
      try {
        cache.load(cacheFile)
        Some(cache)
      } catch {
        case e: BufferIntegrityException => None
      }
    } else {
      None
    }
  }

  def writeFile(filename: File, data: ByteBuffer): Unit = {
    val file = filename.getCanonicalFile.getAbsoluteFile
    file.getParentFile.mkdirs()

    val hash = BufferHash.hashBuffer(data)
    val size = data.remaining
    val cacheFile = getCacheFilePath(file)

    if (opts.skipWriteOnHash) {
      readCacheFile(cacheFile) match {
        case Some(cache) =>
          if (cache.outputHash == hash && cache.outputSize == size) {
            if (opts.verbose) println(s"> Output: ${dataRelative(file)}: Skipped (hash matched)")
            return
          }
        case None =>
      }

      if (opts.verbose) println(s"> Output: ${dataRelative(file)}: ${data.remaining} bytes")
      data.writeToFile(file)
    }

    cacheFile.getParentFile.mkdirs()

    val newCache = new OutputCacheFile()
    newCache.outputHash = hash
    newCache.outputSize = size
    newCache.save(cacheFile)
  }

}
