package game.system.rendering

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable

import core._
import game.system.Entity
import util.geometry._
import CullingSystem._
import CullingSystemImpl._

object CullingSystem {

  val MaskRender = 0x01
  val MaskShadow = 0x02
  val MaskLight = 0x04

}

sealed trait CullingSystem {

  /**
    * Update the bounding areas of dynamically culled objects.
    */
  def updateDynamicCullables(): Unit

  /**
    * Find all entities with bounding areas tagged with `mask` which intersect
    * with `frustum`.
    */
  def cullEntities(frustum: Frustum, mask: Int): Array[Entity]

  /** Attach an axis aligned bounding box to the entity. */
  def addAabb(entity: Entity, aabb: Aabb, mask: Int): Unit

  /** Attach a bounding sphere to the entity. */
  def addSphere(entity: Entity, sphere: Sphere, mask: Int): Unit

  /** Remove an entity from the system. */
  def removeEntity(entity: Entity): Unit
}

object CullingSystemImpl {
  val PoolAabb = 1
  val PoolSphere = 2

  val MaskTreeRender = MaskRender | MaskShadow
  val MaskTreeLight = MaskLight

  final case class ContainerRef(container: CullableContainer, pool: Int, index: Int) {
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

  final class Cullable(val entity: Entity) {
    val refs = new ArrayBuffer[ContainerRef]()
    var aabb: ShapeAabb = null
    var sphere: ShapeSphere = null

    var lastPassAdded: Int = -1

    var dynamicIndex: Int = -1
  }

  final class ShapeAabb(val cullable: Cullable, var localAabb: Aabb, var aabb: Aabb, var mask: Int, val next: ShapeAabb) {
    var lastPassAdded: Int = -1
  }

  final class ShapeSphere(val cullable: Cullable, var localSphere: Sphere, var sphere: Sphere, var mask: Int, val next: ShapeSphere) {
    var lastPassAdded: Int = -1
  }

  final class CullableContainer {
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

  val entityToCullable = new mutable.HashMap[Entity, Cullable]()
  val dynamicCullables = new ArrayPool[Cullable]()

  def getOrAddCullable(entity: Entity): Cullable = {
    val cullable = entityToCullable.getOrElseUpdate(entity, {
      entity.flag0 |= Entity.Flag0_HasCullables
      new Cullable(entity)
    })

    if (!entity.static && cullable.dynamicIndex < 0) {
      cullable.dynamicIndex = dynamicCullables.add(cullable)
    }

    cullable
  }
}

final class CullingSystemImpl extends CullingSystem {

  val narrowAabbs = new ArrayBuffer[ShapeAabb]()
  val narrowSpheres = new ArrayBuffer[ShapeSphere]()

  var currentPass: Int = 0

  val globalContainer = new CullableContainer()

  val QuadTreeBranchAtNumShapes = 16
  val QuadTreeMaxSubdivisionLevel = 8
  val QuadTreeOrigin = Vector3(0.0, 20.0, 0.0)
  val QuadTreeSize = Vector3(500.0, 50.0, 500.0)

  val quadTreeRender = new QuadTreeNode(Aabb(QuadTreeOrigin, QuadTreeSize * 0.5), 1)
  val quadTreeLight = new QuadTreeNode(Aabb(QuadTreeOrigin, QuadTreeSize * 0.5), 1)

  class QuadTreeNode(val bounds: Aabb, val level: Int) {
    val maxOfSizeXZ = math.max(bounds.halfSize.x, bounds.halfSize.z)
    var cullables = new CullableContainer()

    def shapeAdded(): Unit = {
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
  def addToNarrow(container: CullableContainer, mask: Int): Unit = {

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
  def cullQuadTreeToNarrow(node: QuadTreeNode, frustum: Frustum, mask: Int): Unit = {
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
    * Perform broad phase culling: Go through all the high-level CullableContainers
    * and add their contents to the narrow pahse if they are visible.
    */
  def cullBroadPhase(frustum: Frustum, mask: Int): Unit = {

    // Always add global objects
    addToNarrow(globalContainer, mask)

    // Add visible quad-tree objects
    if ((mask & MaskTreeRender) != 0)
      cullQuadTreeToNarrow(quadTreeRender, frustum, mask)
    if ((mask & MaskTreeLight) != 0)
      cullQuadTreeToNarrow(quadTreeLight, frustum, mask)

  }

  /**
    * Perform narrow phase culling: Go through all broad-phase survivors and cull
    * them individiually.
    */
  def cullNarrowPhase(results: ArrayBuffer[Entity], frustum: Frustum): Unit = {

    // Cull AABBs
    {
      var ix = 0
      val num = narrowAabbs.length
      while (ix < num) {
        val shape = narrowAabbs(ix)
        if (frustum.intersects(shape.aabb)) {
          results += shape.cullable.entity
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
          results += shape.cullable.entity
        }
        ix += 1
      }
    }

    narrowAabbs.clear()
    narrowSpheres.clear()
  }

  def addShape(shape: ShapeAabb): Unit = {
    var failedInsert = false

    shape.aabb = shape.localAabb.copy(center = shape.localAabb.center + shape.cullable.entity.position)

    if ((shape.mask & MaskTreeRender) != 0) {
      if (quadTreeRender.bounds.contains(shape.aabb))
        quadTreeRender.add(shape)
      else
        failedInsert = true
    }

    if ((shape.mask & MaskTreeLight) != 0) {
      if (quadTreeLight.bounds.contains(shape.aabb))
        quadTreeLight.add(shape)
      else
        failedInsert = true
    }

    if (failedInsert)
      globalContainer.add(shape)
  }

  def addShape(shape: ShapeSphere): Unit = {
    var failedInsert = false

    shape.sphere = shape.localSphere.copy(center = shape.localSphere.center + shape.cullable.entity.position)

    if ((shape.mask & MaskTreeRender) != 0) {
      if (quadTreeRender.bounds.contains(shape.sphere))
        quadTreeRender.add(shape)
      else
        failedInsert = true
    }

    if ((shape.mask & MaskTreeLight) != 0) {
      if (quadTreeLight.bounds.contains(shape.sphere))
        quadTreeLight.add(shape)
      else
        failedInsert = true
    }

    if (failedInsert)
      globalContainer.add(shape)
  }

  override def updateDynamicCullables(): Unit = {

    {
      var ix = 0
      val len = dynamicCullables.sparseData.length
      while (ix < len) {
        val cullable = dynamicCullables.sparseData(ix)
        if (cullable != null) {

          var refIx = 0
          while (refIx < cullable.refs.length) {
            cullable.refs(refIx).release()
            refIx += 1
          }

          cullable.refs.clear()

          var aabb = cullable.aabb
          while (aabb != null) {
            addShape(aabb)
            aabb = aabb.next
          }

          var sphere = cullable.sphere
          while (sphere != null) {
            addShape(sphere)
            sphere = sphere.next
          }

        }
        ix += 1
      }
    }

  }

  override def cullEntities(frustum: Frustum, mask: Int): Array[Entity] = ???

  override def addAabb(entity: Entity, aabb: Aabb, mask: Int): Unit = {
    val cullable = getOrAddCullable(entity)

    cullable.aabb = new ShapeAabb(cullable, aabb, null, mask, cullable.aabb)
    if (cullable.dynamicIndex < 0)
      addShape(cullable.aabb)
  }

  override def addSphere(entity: Entity, sphere: Sphere, mask: Int): Unit = {
    val cullable = getOrAddCullable(entity)

    cullable.sphere = new ShapeSphere(cullable, sphere, null, mask, cullable.sphere)
    if (cullable.dynamicIndex < 0)
      addShape(cullable.sphere)
  }

  override def removeEntity(entity: Entity): Unit = {
    val cullable = entityToCullable(entity)

    for (ref <- cullable.refs) ref.release()

    if (cullable.dynamicIndex >= 0)
      dynamicCullables.remove(cullable.dynamicIndex)

    entityToCullable.remove(entity)
  }

}

