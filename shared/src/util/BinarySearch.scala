package util

object BinarySearch {

  /**
    * Find the first index where `predicate` returns true in [first, last[. This algorithm expects
    * that the result `predicate` is partitioned in a way that there is a constant
    * `N` for which the function returns false for argument `x < N` and true for `x >= N`.
    * `N` does not need to be included in the range [first, last[ so the function may return
    * `false` or `true` for whatever index. In case every index evaluates to `false` then
    * `last` is returned.
    *
    * @param first Inclusive first index of the search
    * @param last Exclusive last index of the search
    * @param predicate Function to search in
    * @return The first index that `predicate` returns `true` for, `last` if not found
    */
  def upperBound(begin: Int, end: Int, predicate: Int => Boolean): Int = {
    var first = begin
    var count = end
    while (count > 0) {
      val step = count >> 1
      val it = first + step
      if (!predicate(it)) {
        first = it + 1
        count -= step + 1
      } else {
        count = step
      }
    }
    first
  }

}
