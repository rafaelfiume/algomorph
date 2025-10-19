package sorting

import scala.reflect.ClassTag

object CountingSort:

  /**
   * Sort numbers in linear time.
   *
   * Use it when the input range is dense.
   *
   * Stable: Yes. In-Place: No.
   *
   * ===Algorithm===
   * Let `input` be a sequence of non-negative numbers, `min` be the minimum number in the sequence, and `max` be the maximum
   * number/
   *
   * 1) Compute the range size:
   *   - `k = max - min + 1`
   *
   * 2)`Create an array `count` with size `k`, and track the frequency of each `input` number:
   *   - `count[i] = number of occurrences of (i + min) in the input`
   *
   * 3) Compute the cumulative frequency:
   *   - `count[i] = numbers of elements <= (i + min)`.
   *
   * 4) Traverse the `input` in reverse order and place each element in its correct sorted position using the `count` array:
   * sequence.
   * ```
   *   val index = count[input[i] - min] - 1
   *   sorted(index) = input[i]
   *   count[input[i] - min] -= 1
   * ```
   *
   * 5) Return `sorted`.
   *
   * Note on Support for Negative Numbers:
   *   - `index = elem - min` acts as bijective mapping between [min, max] and [0, k)
   *   - `min` maps to `0`
   *   - `max` maps to `k` (range)
   *   - All intermediate values map to consecutive integers in [0, k).
   *
   * ===Evaluation Semantics===
   * {{{
   * Sort.counting(List(15, 9, 12, 7, 9, 7))
   *
   * Initial:
   *   min = 7
   *   max = 15
   *   k = max - min + 1 = 9
   *
   * Frequency:
   *            7   8   9  10  11  12  13  14  15
   *   count = [2,  0,  2,  0,  0,  1,  0,  0,  1]
   *            0   1   2   3   4   5   6   7   8
   *
   * Cumulative count: count[i] <= i + min:
   *            7   8   9  10  11  12  13  14  15
   *   count = [2,  2,  4,  4,  4,  5,  5,  5,  6]
   *            0   1   2   3   4   5   6   7   8
   *
   * Iter 1 (elem = 7):
   *   count[elem - min] == count[7 - 7] == count[0] == 2
   *   sorted: [_, 7, _, _, _, _]
   *   count = [1,  2,  4,  4,  4,  5,  5,  5,  6]
   *
   * Iter 2 (elem = 9):
   *   count[elem - min] == count[9 - 7] == count[2] == 4
   *   sorted: [_, 7, _, 9, _, _]
   *   count = [1,  2,  3,  4,  4,  5,  5,  5,  6]
   *
   * Iter 3 (elem = 7):
   *   count[elem - min] == count[7 - 7] == count[0] == 1
   *   sorted: [7, 7, _, 9, _, _]
   *   count = [0,  2,  3,  4,  4,  5,  5,  5,  6]
   *
   * . . .
   *
   * Iter 6 (elem = 15):
   *   count[elem - min] == count[15 - 7] == count[8] == 6
   *   sorted: [7, 7, 9, 9, 12, 15]
   *   count = [0, 2, 2, 4, 4, 4, 5, 5, 5]
   * }}}
   *
   * Return `sorted`: [7, 7, 9, 9, 12, 15]
   *
   * ===Complexity===
   *   - Time: Θ(n + k)
   *   - Space: Θ(n + k)
   */
  def sort[T: ClassTag](numbers: Seq[T])(using num: Integral[T]): Seq[T] =
    import num.*
    val zero = num.fromInt(0)
    val one = num.fromInt(1)

    if numbers.size <= 1 then numbers
    else
      val (minimum, maximum) = numbers.foldLeft((numbers.head, numbers.head)) { case (minValue, maxValue) -> elem =>
        (num.min(minValue, elem), num.max(maxValue, elem))
      }
      val range = maximum.toLong - minimum.toLong + 1
      require(range < Int.MaxValue, s"range must be < Int.MaxValue")

      val k = range.toInt

      val count = Array.fill[T](k)(zero)
      numbers.foreach { n =>
        count((n - minimum).toInt) += one
      }

      for i <- 1 until k do count(i) = count(i) + count(i - 1)

      val sorted = new Array[T](numbers.size) // Uninitialised array saves an O(n) pass
      numbers.reverse.foreach { elem =>
        val index = count((elem - minimum).toInt).toInt - 1
        sorted(index) = elem
        count((elem - minimum).toInt) -= one
      }
      sorted.toIndexedSeq // Based on immutable.ArraySeq.unsafeWrapArray, which is just a wrapper O(1)
