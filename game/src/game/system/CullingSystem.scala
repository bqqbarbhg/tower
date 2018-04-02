package game.system

import core._
import ui.DebugDraw
import util.geometry._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object CullingSystem {

  private val PoolAabb = 1
  private val PoolSphere = 2

  private final case class ContainerRef(container: CullableContainer, pool: Int, index: Int) {
    def release(): Unit = {
      if (pool == PoolAabb) {
        container.aabbList.remove(index)
      } else if (pool == PoolSphere) {
        container.sphereList.remove(index)
      } else {
        throw new RuntimeException(s"Unknown pool: $pool")
      }
    }
  }

  private final class Cullable(val entity: Entity) {
    val refs = new ArrayBuffer[ContainerRef]()
    var lastPassAdded: Int = -1
  }

  private final class ShapeAabb(val cullable: Cullable, var aabb: Aabb, var mask: Int) {
    var lastPassAdded: Int = -1
  }

  private final class ShapeSphere(val cullable: Cullable, var sphere: Sphere, var mask: Int) {
    var lastPassAdded: Int = -1
  }

  private final class CullableContainer {
    val aabbList = new ArrayPool[ShapeAabb]()
    val sphereList = new ArrayPool[ShapeSphere]()

    def add(shape: ShapeAabb): Unit = {
      val ix = aabbList.add(shape)
      shape.cullable.refs += ContainerRef(this, PoolAabb, ix)
    }

    def add(shape: ShapeSphere): Unit = {
      val ix = sphereList.add(shape)
      shape.cullable.refs += ContainerRef(this, PoolSphere, ix)
    }

    def clear(): Unit = {
      for ((shape, index) <- aabbList.sparseData.zipWithIndex) {
        if (shape != null) {
          val ref = ContainerRef(this, PoolAabb, index)
          shape.cullable.refs -= ref
        }
      }

      for ((shape, index) <- sphereList.sparseData.zipWithIndex) {
        if (shape != null) {
          val ref = ContainerRef(this, PoolSphere, index)
          shape.cullable.refs -= ref
        }
      }

      aabbList.clear()
      sphereList.clear()
    }

    def numShapes: Int = aabbList.size + sphereList.size
  }

  private val entityToCullable = new mutable.HashMap[Entity, Cullable]()

  private def getCullable(entity: Entity): Cullable = {
    entityToCullable.getOrElseUpdate(entity, new Cullable(entity))
  }

  private val narrowAabbs = new ArrayBuffer[ShapeAabb]()
  private val narrowSpheres = new ArrayBuffer[ShapeSphere]()

  private var currentPass: Int = 0

  private val globalContainer = new CullableContainer()

  private val QuadTreeBranchAtNumShapes = 16
  private val QuadTreeMaxSubdivisionLevel = 8
  private val QuadTreeOrigin = Vector3(0.0, 20.0, 0.0)
  private val QuadTreeSize = Vector3(500.0, 50.0, 500.0)

  private val quadTree = new QuadTreeNode(Aabb(QuadTreeOrigin, QuadTreeSize * 0.5), 1)

  private class QuadTreeNode(val bounds: Aabb, val level: Int) {
    val maxOfSizeXZ = math.max(bounds.halfSize.x, bounds.halfSize.z)
    var cullables = new CullableContainer()

    private def shapeAdded(): Unit = {
      if (isLeaf && level < QuadTreeMaxSubdivisionLevel && cullables.numShapes >= QuadTreeBranchAtNumShapes) {
        subdivide()
      }
    }

    def subdivide(): Unit = {
      if (!isLeaf) return

      val oldCullables = cullables
      cullables = new CullableContainer()

      val cc = bounds.center
      val cs = bounds.halfSize *@ Vector3(0.5, 1.0, 0.5)
      val cl = level + 1
      c00 = new QuadTreeNode(Aabb(cc + Vector3(-cs.x, 0.0, -cs.z), cs), cl)
      c01 = new QuadTreeNode(Aabb(cc + Vector3(+cs.x, 0.0, -cs.z), cs), cl)
      c10 = new QuadTreeNode(Aabb(cc + Vector3(-cs.x, 0.0, +cs.z), cs), cl)
      c11 = new QuadTreeNode(Aabb(cc + Vector3(+cs.x, 0.0, +cs.z), cs), cl)

      for (c <- oldCullables.aabbList)   this.add(c)
      for (c <- oldCullables.sphereList) this.add(c)

      oldCullables.clear()
    }

    def add(shape: ShapeAabb): Unit = {
      if (bounds.intersects(shape.aabb)) {
        val hs = shape.aabb.halfSize
        if (isLeaf || math.min(hs.x, hs.z) >= maxOfSizeXZ) {
          cullables.add(shape)
          shapeAdded()
        } else {
          c00.add(shape); c01.add(shape)
          c10.add(shape); c11.add(shape)
        }
      }
    }

    def add(shape: ShapeSphere): Unit = {
      if (bounds.intersects(shape.sphere)) {
        val hs = shape.sphere.radius
        if (isLeaf || hs >= maxOfSizeXZ) {
          cullables.add(shape)
          shapeAdded()
        } else {
          c00.add(shape); c01.add(shape)
          c10.add(shape); c11.add(shape)
        }
      }
    }

    var c00: QuadTreeNode = null
    var c01: QuadTreeNode = null
    var c10: QuadTreeNode = null
    var c11: QuadTreeNode = null

    def isLeaf: Boolean = c00 == null
  }

  /**
    * Add contents of `container` to the narrow phase.
    */
  private def addToNarrow(container: CullableContainer, mask: Int): Unit = {

    // Add AABBs
    {
      var ix = 0
      val sd = container.aabbList.sparseData
      var num = sd.length
      while (ix < num) {
        val shape = sd(ix)
        if (shape != null && (shape.mask & mask) != 0 && shape.lastPassAdded != currentPass) {
          shape.lastPassAdded = currentPass
          narrowAabbs += shape
        }
        ix += 1
      }
    }

    // Add spheres
    {
      var ix = 0
      val sd = container.sphereList.sparseData
      var num = sd.length
      while (ix < num) {
        val shape = sd(ix)
        if (shape != null && (shape.mask & mask) != 0 && shape.lastPassAdded != currentPass) {
          shape.lastPassAdded = currentPass
          narrowSpheres += shape
        }
        ix += 1
      }
    }

  }

  /**
    * Add contents of `node` to the narrow phase if the quadtree node interesects
    * with `frustum`.
    */
  private def cullQuadTreeToNarrow(node: QuadTreeNode, frustum: Frustum, mask: Int): Unit = {
    if (!frustum.intersects(node.bounds)) return

    addToNarrow(node.cullables, mask)

    if (!node.isLeaf) {
      cullQuadTreeToNarrow(node.c00, frustum, mask)
      cullQuadTreeToNarrow(node.c01, frustum, mask)
      cullQuadTreeToNarrow(node.c10, frustum, mask)
      cullQuadTreeToNarrow(node.c11, frustum, mask)
    }
  }

  /**
    * Add `cullable` to the visible set (mark it as visible).
    */
  private def addToVisible(cullable: Cullable): Unit = {
    if (cullable.lastPassAdded == currentPass) return

    cullable.lastPassAdded = currentPass
  }


  /**
    * Perform broad phase culling: Go through all the high-level CullableContainers
    * and add their contents to the narrow pahse if they are visible.
    */
  private def cullBroadPhase(view: Viewport): Unit = {

    // Always add global objects
    addToNarrow(globalContainer, view.viewportMask)

    // Add visible quad-tree objects
    cullQuadTreeToNarrow(quadTree, view.frustum, view.viewportMask)

  }

  /**
    * Perform narrow phase culling: Go through all broad-phase survivors and cull
    * them individiually.
    */
  private def cullNarrowPhase(view: Viewport): Unit = {
    val frustum = view.frustum

    // Cull AABBs
    {
      var ix = 0
      val num = narrowAabbs.length
      while (ix < num) {
        val shape = narrowAabbs(ix)
        if (frustum.intersects(shape.aabb)) {
          view.visibleEntities += shape.cullable.entity
        }
        ix += 1
      }
    }

    // Cull spheres
    {
      var ix = 0
      val num = narrowSpheres.length
      while (ix < num) {
        val shape = narrowSpheres(ix)
        if (frustum.intersects(shape.sphere)) {
          view.visibleEntities += shape.cullable.entity
        }
        ix += 1
      }
    }

    narrowAabbs.clear()
    narrowSpheres.clear()
  }

  def update(): Unit = {
    for (viewport <- viewports) {
      currentPass += 1
      viewport.visibleEntities.clear()
      cullBroadPhase(viewport)
      cullNarrowPhase(viewport)
    }
  }

  class Viewport {
    var frustum: Frustum = null
    var viewportMask: Int = 0

    val visibleEntities = new ArrayBuffer[Entity]()
  }

  var viewports = ArrayBuffer[Viewport]()

  def addStaticAABB(entity: Entity, relativeAabb: Aabb, viewportMask: Int): Unit = {
    val aabb = relativeAabb.copy(center = entity.position + relativeAabb.center)

    val cullable = getCullable(entity)
    val shape = new ShapeAabb(cullable, aabb, viewportMask)

    if (quadTree.bounds.contains(aabb)) {
      quadTree.add(shape)
    } else {
      globalContainer.add(shape)
    }
  }

  def addStaticSphere(entity: Entity, relativeSphere: Sphere, viewportMask: Int): Unit = {
    val sphere = relativeSphere.copy(center = entity.position + relativeSphere.center)

    val cullable = getCullable(entity)
    val shape = new ShapeSphere(cullable, sphere, viewportMask)

    if (quadTree.bounds.contains(sphere)) {
      quadTree.add(shape)
    } else {
      globalContainer.add(shape)
    }
  }

  def debugDrawQuadTree(): Unit = {
    currentPass += 1

    def debugDrawNode(node: QuadTreeNode): Unit = {
      if (!node.isLeaf) {
        debugDrawNode(node.c00)
        debugDrawNode(node.c01)
        debugDrawNode(node.c10)
        debugDrawNode(node.c11)
      } else {
        val c = node.bounds.center.copy(y = 6.0)
        val h = node.bounds.halfSize.copy(y = 4.0)
        val aabb = Aabb(c, h)
        val color = if (viewports.head.frustum.intersects(node.bounds))
          Color.rgb(0xFF8888) else Color.rgb(0xFF0000)
        DebugDraw.drawAabb(aabb, color)
      }
    }

    debugDrawNode(quadTree)
  }

}


