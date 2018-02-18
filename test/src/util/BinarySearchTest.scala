package util

import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalactic.TolerantNumerics
import io.SimpleSerialization._

import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class BinarySearchTest extends FlatSpec with Matchers {

  "BinarySearch.upperBound" should "find index in trivial case" in {
    val index = BinarySearch.upperBound(0, 10000, index => index >= 123)
    assert(index === 123)
  }

  it should "find the first element in case of duplicates" in {
    val array = Array(1, 2, 3, 3, 3, 3, 4, 5, 6)
    val index = BinarySearch.upperBound(0, array.length, index => array(index) >= 3)
    assert(index === 2)
  }

  it should "return the max index if not found" in {
    val array = Array(1, 2, 3, 3, 3, 3, 4, 5, 6)
    val index = BinarySearch.upperBound(0, array.length, index => array(index) >= 10)
    assert(index === array.length)
  }
}
