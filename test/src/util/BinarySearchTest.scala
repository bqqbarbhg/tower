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

  it should "return the max index if not found (zero begin)" in {
    val array = Array(1, 2, 3, 3, 3, 3, 4, 5, 6)
    val index = BinarySearch.upperBound(0, array.length, index => array(index) >= 10)
    assert(index === array.length)
  }

  it should "return the max index if not found (non-zero begin)" in {
    val array = Array(1, 2, 3, 3, 3, 3, 4, 5, 6)
    val index = BinarySearch.upperBound(5, array.length, index => array(index) >= 10)
    assert(index === array.length)
  }

  "BinarySearch.upperBoundRight" should "find index in trivial case" in {
    val index = BinarySearch.upperBoundRight(0, 10000, index => index <= 123)
    assert(index === 123)
  }

  it should "find the last element in case of duplicates" in {
    val array = Array(1, 2, 3, 3, 3, 3, 4, 5, 6)
    val index = BinarySearch.upperBoundRight(0, array.length, index => array(index) <= 3)
    assert(index === 5)
  }

  it should "return one before begin if not found (zero begin)" in {
    val array = Array(2, 3, 4, 4, 4, 4, 5, 6, 7)
    val index = BinarySearch.upperBoundRight(0, array.length, index => array(index) <= 1)
    assert(index === -1)
  }

  it should "return one before begin if not found (non-zero begin)" in {
    val array = Array(2, 3, 4, 4, 4, 4, 5, 6, 7)
    val index = BinarySearch.upperBoundRight(4, array.length, index => array(index) <= 3)
    assert(index === 3)
  }
}
