package game.system.rendering

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import core._
import game.system._
import game.system.Entity._
import util.geometry._
import CullingSystem._
import CullingSystemImpl._
import io.property._
import ui.DebugDraw

object CullingSystem {

  case class RayHit(entity: Entity, t: Double)

  val MaskRender = 1 << 0
  val MaskShadow = 1 << 1
  val MaskLight = 1 << 2

  val MaskTower = 1 << 3
  val MaskEnemy = 1 << 4
  val MaskAnyGameplay = MaskTower | MaskEnemy

  val NameToFlag = Map(
    "Render" -> MaskRender,
    "Shadow" -> MaskShadow,
    "Light" -> MaskLight,
    "Tower" -> MaskTower,
    "Enemy" -> MaskEnemy,
  )

  object MaskConverter extends PropertySet.CustomConverter {
    override def set(inst: PropertyContainer, prop: Property, value: String): Unit = {
      val split = value.split(' ')
      val flags = split.foldLeft(0)((flag, str) => flag | NameToFlag(str))
      val p = prop.asInstanceOf[IntProp]
      p.set(inst, flags)
    }
  }

}

sealed trait CullingSystem extends EntityDeleteListener {

  /**
    * Update the bounding areas of dynamically culled objects.
    */
  def updateDynamicCullables(): Unit

  /**
    * Find all entities with bounding areas tagged with `mask` which intersect
    * with `frustum`. The resutls are gathered to `set`.
    */
  def cullEntities(set: EntitySet, frustum: Frustum, mask: Int): Unit

  /** Attach an axis aligned bounding box to the entity. */
  def addAabb(entity: Entity, aabb: Aabb, mask: Int): Unit

  /** Attach a bounding sphere to the entity. */
  def addSphere(entity: Entity, sphere: Sphere, mask: Int): Unit

  /** Add an entity that is always visible */
  def addAlwaysVisible(entity: Entity, mask: Int): Unit

  /** Remove an entity from the system. */
  def removeCullables(entity: Entity): Unit

  /** Query AABB and return intersecting entities to existing `results` */
  def queryAabb(aabb: Aabb, maxResults: Int, mask: Int, results: ArrayBuffer[Entity]): Unit

  /** Query AABB and return intersecting entities */
  def queryAabb(aabb: Aabb, maxResults: Int, mask: Int): ArrayBuffer[Entity] = {
    val results = new ArrayBuffer[Entity](math.min(maxResults, 32))
    queryAabb(aabb, maxResults, mask, results)
    results
  }

  /** Cast a ray and return _arbitrary_ intersecting objects (with hit distance) to existing `results`. */
  def queryRay(ray: Ray, maxDistance: Double, maxResults: Int, mask: Int, results: ArrayBuffer[RayHit]): Unit

  /** Cast a ray and return _arbitrary_ intersecting objects (with hit distance). */
  def queryRay(ray: Ray, maxDistance: Double, maxResults: Int, mask: Int): ArrayBuffer[RayHit] = {
    val results = new ArrayBuffer[RayHit](math.min(maxResults, 32))
    queryRay(ray, maxDistance, maxResults, mask, results)
    results
  }

  /** Draw a debug view of the culling bounds and partitioning structure for a mask */
  def debugDrawCulling(frustum: Frustum, mask: Int): Unit
}

object CullingSystemImpl {
  val PoolAabb = 1
  val PoolSphere = 2

  val MaskTreeRender = MaskRender | MaskShadow
  val MaskTreeLight = MaskLight
  val MaskTreeGameplay = MaskAnyGameplay

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
    var alwaysVisibleIndex: Int = -1
  }

  final class ShapeAabb(val cullable: Cullable, var localAabb: Aabb, var aabb: Aabb, var mask: Int, val next: ShapeAabb) {
    var lastPassAdded: Int = -1
  }

  final class ShapeSphere(val cullable: Cullable, var localSphere: Sphere, var sphere: Sphere, var mask: Int, val next: ShapeSphere) {
    var lastPassAdded: Int = -1
  }

  final class AlwaysVisible(val cullable: Cullable, var mask: Int) {
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

}

final class CullingSystemImpl extends CullingSystem {

  val entityToCullable = new mutable.HashMap[Entity, Cullable]()
  val dynamicCullables = new ArrayPool[Cullable]()
  val alwaysVisible = new ArrayPool[AlwaysVisible]()

  def getOrAddCullable(entity: Entity): Cullable = {
    val cullable = entityToCullable.getOrElseUpdate(entity, {
      entity.setFlag(Flag_Cullables)
      new Cullable(entity)
    })

    if (!entity.static && cullable.dynamicIndex < 0) {
      cullable.dynamicIndex = dynamicCullables.add(cullable)
    }

    cullable
  }

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
  val quadTreeGameplay = new QuadTreeNode(Aabb(QuadTreeOrigin, QuadTreeSize * 0.5), 1)

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

  def addCullableToSet(set: EntitySet, cullable: Cullable): Unit = {
    if (cullable.lastPassAdded == currentPass) return
    cullable.lastPassAdded = currentPass
    set.add(cullable.entity)
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
    if ((mask & MaskTreeGameplay) != 0)
      cullQuadTreeToNarrow(quadTreeGameplay, frustum, mask)

  }

  /**
    * Perform narrow phase culling: Go through all broad-phase survivors and cull
    * them individiually.
    */
  def cullNarrowPhase(set: EntitySet, frustum: Frustum, mask: Int): Unit = {

    // Cull AABBs
    {
      var ix = 0
      val num = narrowAabbs.length
      while (ix < num) {
        val shape = narrowAabbs(ix)
        if (frustum.intersects(shape.aabb)) {
          addCullableToSet(set, shape.cullable)
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
          addCullableToSet(set, shape.cullable)
        }
        ix += 1
      }
    }

    // Always visible
    {
      var ix = 0
      val num = alwaysVisible.sparseData.length
      while (ix < num) {
        val always = alwaysVisible.sparseData(ix)
        if (always != null && (always.mask & mask) != 0) {
          addCullableToSet(set, always.cullable)
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

    if ((shape.mask & MaskTreeGameplay) != 0) {
      if (quadTreeGameplay.bounds.contains(shape.aabb))
        quadTreeGameplay.add(shape)
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

    if ((shape.mask & MaskTreeGameplay) != 0) {
      if (quadTreeGameplay.bounds.contains(shape.sphere))
        quadTreeGameplay.add(shape)
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

  override def cullEntities(set: EntitySet, frustum: Frustum, mask: Int): Unit = {
    val res = new ArrayBuffer[Entity]()
    currentPass += 1
    cullBroadPhase(frustum, mask)
    cullNarrowPhase(set, frustum, mask)
    res.toArray
  }

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

  override def addAlwaysVisible(entity: Entity, mask: Int): Unit = {
    val cullable = getOrAddCullable(entity)

    if (cullable.alwaysVisibleIndex < 0) {
      cullable.alwaysVisibleIndex = alwaysVisible.add(new AlwaysVisible(cullable, mask))
    }
  }

  override def removeCullables(entity: Entity): Unit = {
    val cullable = entityToCullable(entity)

    for (ref <- cullable.refs) ref.release()

    if (cullable.dynamicIndex >= 0)
      dynamicCullables.remove(cullable.dynamicIndex)

    if (cullable.alwaysVisibleIndex >= 0)
      alwaysVisible.remove(cullable.alwaysVisibleIndex)

    entityToCullable.remove(entity)

    entity.clearFlag(Flag_Cullables)
  }

  override def entitiesDeleted(entities: EntitySet): Unit = entities.flag(Flag_Cullables).foreach(removeCullables)

  def addToQueryResults(entity: Entity, results: ArrayBuffer[Entity], maxResults: Int): Boolean = {
    if (results.length >= maxResults) return true
    for (e <- results) if (e == entity) return true
    results += entity
    results.length >= maxResults
  }

  def queryCullableAabb(cullables: CullableContainer, queryShape: Aabb, results: ArrayBuffer[Entity], maxResults: Int, mask: Int): Unit = {
    for (aabb <- cullables.aabbList) {
      if ((aabb.mask & mask) != 0 && aabb.aabb.intersects(queryShape)) {
        if (addToQueryResults(aabb.cullable.entity, results, maxResults)) return
      }
    }

    for (sphere <- cullables.sphereList) {
      if ((sphere.mask & mask) != 0 && sphere.sphere.intersects(queryShape)) {
        if (addToQueryResults(sphere.cullable.entity, results, maxResults)) return
      }
    }
  }

  def queryQuadTreeAabb(node: QuadTreeNode, queryShape: Aabb, results: ArrayBuffer[Entity], maxResults: Int, mask: Int): Unit = {
    if (results.length >= maxResults || !node.bounds.intersects(queryShape)) return

    queryCullableAabb(node.cullables, queryShape, results, maxResults, mask)

    if (!node.isLeaf) {
      queryQuadTreeAabb(node.c00, queryShape, results, maxResults, mask)
      queryQuadTreeAabb(node.c01, queryShape, results, maxResults, mask)
      queryQuadTreeAabb(node.c10, queryShape, results, maxResults, mask)
      queryQuadTreeAabb(node.c11, queryShape, results, maxResults, mask)
    }
  }

  override def queryAabb(aabb: Aabb, maxResults: Int, mask: Int, results: ArrayBuffer[Entity]): Unit = {
    queryCullableAabb(globalContainer, aabb, results, maxResults, mask)

    if ((mask & MaskTreeRender) != 0)
      queryQuadTreeAabb(quadTreeRender, aabb, results, maxResults, mask)
    if ((mask & MaskTreeLight) != 0)
      queryQuadTreeAabb(quadTreeLight, aabb, results, maxResults, mask)
    if ((mask & MaskTreeGameplay) != 0)
      queryQuadTreeAabb(quadTreeGameplay, aabb, results, maxResults, mask)
  }

  def addToRayQueryResults(entity: Entity, t: Double, results: ArrayBuffer[RayHit], maxResults: Int): Boolean = {
    if (results.length >= maxResults) return true
    for (e <- results) if (e.entity == entity) return true
    results += RayHit(entity, t)
    results.length >= maxResults
  }

  def queryCullableRay(cullables: CullableContainer, ray: Ray, maxDistance: Double, results: ArrayBuffer[RayHit], maxResults: Int, mask: Int): Unit = {
    for (aabb <- cullables.aabbList) {
      if ((aabb.mask & mask) != 0) {
        for (t <- ray.intersect(aabb.aabb, maxDistance)) {
          if (addToRayQueryResults(aabb.cullable.entity, t, results, maxResults)) return
        }
      }
    }

    for (sphere <- cullables.sphereList) {
      if ((sphere.mask & mask) != 0) {
        for (t <- ray.intersect(sphere.sphere, maxDistance)) {
          if (addToRayQueryResults(sphere.cullable.entity, t, results, maxResults)) return
        }
      }
    }
  }

  def queryQuadTreeRay(node: QuadTreeNode, ray: Ray, maxDistance: Double, results: ArrayBuffer[RayHit], maxResults: Int, mask: Int): Unit = {
    if (results.length >= maxResults || ray.intersect(node.bounds, maxDistance).isEmpty) return

    queryCullableRay(node.cullables, ray, maxDistance, results, maxResults, mask)

    if (!node.isLeaf) {
      queryQuadTreeRay(node.c00, ray, maxDistance, results, maxResults, mask)
      queryQuadTreeRay(node.c01, ray, maxDistance, results, maxResults, mask)
      queryQuadTreeRay(node.c10, ray, maxDistance, results, maxResults, mask)
      queryQuadTreeRay(node.c11, ray, maxDistance, results, maxResults, mask)
    }
  }

  override def queryRay(ray: Ray, maxDistance: Double, maxResults: Int, mask: Int, results: ArrayBuffer[RayHit]): Unit = {
    queryCullableRay(globalContainer, ray, maxDistance, results, maxResults, mask)

    if ((mask & MaskTreeRender) != 0)
      queryQuadTreeRay(quadTreeRender, ray, maxDistance, results, maxResults, mask)
    if ((mask & MaskTreeLight) != 0)
      queryQuadTreeRay(quadTreeLight, ray, maxDistance, results, maxResults, mask)
    if ((mask & MaskTreeGameplay) != 0)
      queryQuadTreeRay(quadTreeGameplay, ray, maxDistance, results, maxResults, mask)
  }

  def debugDrawCullables(cullables: CullableContainer, frustum: Frustum, mask: Int): Unit = {
    val color = Color.rgb(0xFF5555)

    for (aabb <- cullables.aabbList) {
      if ((aabb.mask & mask) != 0 && frustum.intersects(aabb.aabb)) {
        DebugDraw.drawAabb(aabb.aabb, color)
      }
    }

    for (sphere <- cullables.sphereList) {
      if ((sphere.mask & mask) != 0 && frustum.intersects(sphere.sphere)) {
        val s = sphere.sphere
        val r = s.radius
        DebugDraw.drawLine(s.center, Vector3(-r , 0.0, 0.0), color)
        DebugDraw.drawLine(s.center, Vector3(+r , 0.0, 0.0), color)
        DebugDraw.drawLine(s.center, Vector3(0.0, -r , 0.0), color)
        DebugDraw.drawLine(s.center, Vector3(0.0, +r , 0.0), color)
        DebugDraw.drawLine(s.center, Vector3(0.0, 0.0, -r ), color)
        DebugDraw.drawLine(s.center, Vector3(0.0, 0.0, +r ), color)
      }
    }

  }

  def debugDrawQuadTree(node: QuadTreeNode, frustum: Frustum, mask: Int): Unit = {
    if (!frustum.intersects(node.bounds)) return

    val min = node.bounds.min
    val max = node.bounds.max.copy(y = 5.0)
    DebugDraw.drawAabb(min, max, Color.rgb(0x444444))

    debugDrawCullables(node.cullables, frustum, mask)

    if (!node.isLeaf) {
      debugDrawQuadTree(node.c00, frustum, mask)
      debugDrawQuadTree(node.c01, frustum, mask)
      debugDrawQuadTree(node.c10, frustum, mask)
      debugDrawQuadTree(node.c11, frustum, mask)
    }
  }

  override def debugDrawCulling(frustum: Frustum, mask: Int): Unit = {
    debugDrawCullables(globalContainer, frustum, mask)

    if ((mask & MaskTreeRender) != 0)
      debugDrawQuadTree(quadTreeRender, frustum, mask)
    if ((mask & MaskTreeLight) != 0)
      debugDrawQuadTree(quadTreeLight, frustum, mask)
    if ((mask & MaskTreeGameplay) != 0)
      debugDrawQuadTree(quadTreeGameplay, frustum, mask)
  }
}

