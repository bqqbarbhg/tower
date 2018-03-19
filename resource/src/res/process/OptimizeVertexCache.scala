package res.process

import collection.immutable
import collection.mutable
import math.Ordering.Implicits._
import collection.mutable.ArrayBuffer

import res.intermediate._
import res.intermediate.Mesh._

object OptimizeVertexCache {

  class Triangle(val index: Int, val a: Int, val b: Int, val c: Int) extends Ordered[Triangle] {
    import scala.math.Ordered.orderingToOrdered
    var score: Float = 0
    def compare(rhs: Triangle) = (score, index) compare (rhs.score, rhs.index)
  }

  /**
    * Optimize vertex ordering for GPU vertex post-transform cache.
    *
    * Uses Tom Forsyth's Linear-Speed Vertex Cache Optimisation,
    * see https://tomforsyth1000.github.io/papers/fast_vert_cache_opt.html
    */
  def optimizeVertexPostTransformCache(mesh: Mesh): Unit = {
    val CacheSize = 32

    val numVerts = mesh.vertices.length

    val vertexScore = new Array[Float](numVerts)

    val vertexTriangles = Array.fill(numVerts)(new ArrayBuffer[Triangle]())
    val triangleOrdering = mutable.TreeSet[Triangle]()

    val lruCache = new ArrayBuffer[Int]()

    def calculateVertexScore(index: Int): Float = {
      val CacheDecayPower = 1.5
      val LastTriScore = 0.75
      val ValenceBoostScale = 2.0
      val ValenceBoostPower = 0.5

      val valence = vertexTriangles(index).length
      if (valence == 0) return -1.0f

      val cachePos = lruCache.size - lruCache.indexOf(index) - 1
      val baseScore = if (cachePos >= CacheSize) {
        0.0
      } else if (cachePos < 3) {
        LastTriScore
      } else {
        assert(cachePos < CacheSize)
        val scale = 1.0 / (CacheSize - 3)
        val score = 1.0 - (cachePos - 3) * scale
        math.pow(score, CacheDecayPower)
      }

      val valenceBoost = math.pow(valence, -ValenceBoostPower)
      val totalScore = baseScore + valenceBoost * ValenceBoostScale
      totalScore.toFloat
    }

    def calculateTriangleScore(tri: Triangle): Float = vertexScore(tri.a) + vertexScore(tri.b) + vertexScore(tri.c)

    def updateTriangleScore(tri: Triangle): Unit = {
      triangleOrdering -= tri
      tri.score = calculateTriangleScore(tri)
      triangleOrdering += tri
    }

    def updateVertexScore(index: Int): Unit = {
      vertexScore(index) = calculateVertexScore(index)
      for (tri <- vertexTriangles(index)) {
        updateTriangleScore(tri)
      }
    }

    val initialTriangles = (for ((indices, triangleIndex) <- mesh.indices.grouped(3).zipWithIndex) yield {
      val Array(a, b, c) = indices
      val tri = new Triangle(triangleIndex, a, b, c)
      vertexTriangles(a) += tri
      vertexTriangles(b) += tri
      vertexTriangles(c) += tri
      tri
    }).toVector

    for (i <- 0 until numVerts) {
      vertexScore(i) = calculateVertexScore(i)
    }

    for (tri <- initialTriangles) {
      tri.score = calculateTriangleScore(tri)
      triangleOrdering += tri
    }

    val finalTriangles = new ArrayBuffer[Triangle]()

    while (triangleOrdering.nonEmpty) {
      val tri = triangleOrdering.last
      triangleOrdering -= tri
      finalTriangles += tri

      // Remove the triangle from the valence list
      vertexTriangles(tri.a) -= tri
      vertexTriangles(tri.b) -= tri
      vertexTriangles(tri.c) -= tri

      // Potentially remove triangle from cache
      lruCache -= tri.a
      lruCache -= tri.b
      lruCache -= tri.c

      // Insert the triangle to the front of the cache
      lruCache += tri.c
      lruCache += tri.b
      lruCache += tri.a

      // Update affected vertices. Note: The surrounding valence changes will
      // be propageted since this triangle is in the LRU cache for sure!
      for (ix <- lruCache) {
        updateVertexScore(ix)
      }

      // Clamp the cache size
      if (lruCache.size > CacheSize) {
        lruCache.remove(0, lruCache.size - CacheSize)
      }
    }

    mesh.indices = finalTriangles.flatMap(tri => Array(tri.a, tri.b, tri.c)).toArray
  }

  /**
    * Re-order the vertex buffer to index buffer order, this should result in
    * mostly ascending index ordering.
    */
  def optimizeVertexBufferIndex(mesh: Mesh): Unit = {
    val indexRemap = mutable.HashMap[Int, Int]()
    val newVertices = new ArrayBuffer[Vertex]()

    val newIndices = (for (ix <- mesh.indices) yield {
      indexRemap.getOrElseUpdate(ix, {
        val newIndex = indexRemap.size
        newVertices += mesh.vertices(ix)
        newIndex
      })
    })

    mesh.indices = newIndices
    mesh.vertices = newVertices.toArray
  }

}

