package util

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

object AStar {

  trait State[A <: State[A]] {
    /** Valid transitions from this state with actual distance */
    def neighbors: Iterable[(A, Double)]

    /** Estimate of "distance" to the goal state */
    def heuristic: Double

    /** Is this a terminal state */
    def goal: Boolean
  }

  case class OpenState[S <: State[S] : ClassTag](state: S, score: Double, distance: Double, depth: Int, path: List[S])

  class StateOrdering[S <: State[S]] extends Ordering[OpenState[S]] {
    override def compare(x: OpenState[S], y: OpenState[S]): Int = {
      y.score.compareTo(x.score)
    }
  }

  def search[S <: State[S] : ClassTag](initial: S, maxIterations: Int): Array[S] = search(Some(initial), maxIterations)

  def search[S <: State[S] : ClassTag](initial: TraversableOnce[S], maxIterations: Int): Array[S] = {
    val weighted = initial.map((_, 0.0)).toIterable
    search(weighted, maxIterations)
  }

  def search[S <: State[S] : ClassTag](initial: Iterable[(S, Double)], maxIterations: Int): Array[S] = {
    val initialSeq = initial.map(_._1).toSeq
    val firstOpen = initial.toSeq.map(s => OpenState(s._1, s._2, s._2, 1, s._1 :: Nil))

    val openSet = mutable.PriorityQueue(firstOpen : _*)(new StateOrdering[S])
    val closedSet = mutable.HashSet[S](initialSeq : _*)
    var iter = 0

    while (openSet.nonEmpty && iter < maxIterations) {
      iter += 1

      val cur = openSet.dequeue()
      val state = cur.state

      if (state.goal) {
        val res = new Array[S](cur.depth)
        var ix = cur.depth
        var pathNode = cur.path
        while (ix > 0) {
          ix -= 1
          res(ix) = pathNode.head
          pathNode = pathNode.tail
        }
        return res
      }

      for ((next, dist) <- state.neighbors) {
        if (closedSet.add(next)) {
          val nextDist = cur.distance + dist
          val nextScore = nextDist + next.heuristic
          val os = OpenState(next, nextScore, nextDist, cur.depth + 1, next :: cur.path)
          openSet.enqueue(os)
        }
      }
    }

    new Array[S](0)
  }

}


