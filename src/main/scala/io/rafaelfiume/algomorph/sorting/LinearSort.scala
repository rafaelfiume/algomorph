package io.rafaelfiume.algomorph.sorting

import scala.reflect.ClassTag
import scala.annotation.tailrec

object LinearSort:

  def sort[T](numbers: Seq[T])(using num: Integral[T], classTag: ClassTag[T]): Seq[T] =
    if numbers.size <= 1 then numbers
    else
      // Could have used instead of ClassTag:
      // 1) erasedValue - avoided for now because still experimental
      // 2) TypeClass - skipped since goal is a simple Radix.sort utility optimised for the concrete discrete number type
      if classTag.runtimeClass == classOf[Byte] then RadixSort.sortByte(numbers.asInstanceOf[Seq[Byte]]).asInstanceOf[Seq[T]]
      else if classTag.runtimeClass == classOf[Short] then
        RadixSort.sortShort(numbers.asInstanceOf[Seq[Short]]).asInstanceOf[Seq[T]]
      else if classTag.runtimeClass == classOf[Int] then RadixSort.sortInt(numbers.asInstanceOf[Seq[Int]]).asInstanceOf[Seq[T]]
      else if classTag.runtimeClass == classOf[Long] then RadixSort.sortLong(numbers.asInstanceOf[Seq[Long]]).asInstanceOf[Seq[T]]
      else RadixSort.sortIntegral(numbers)

  /**
   * Sorts discrete numbers in linear time.
   *
   * Stable: Yes. In-Place: No.
   *
   * ===Algorithm===
   *
   * Let `input` be a sequence of discrete number and `maxNumDigits` be the number of digits of the maximum element in the
   * sequence.
   *
   * For all numbers in the sequence and for each digit, starting from the least-significant, use counting sort to order the
   * sequence of elements.
   */
  object RadixSort:

    /**
     * 32-bit integers radix sort.
     *
     * Handles negative numbers by providing a bijective mapping between signed [-2^31, 2^31) and unsigned [0, 2^32) ints by
     * flipping sign bit with `XOR Int.MinValue`.
     *
     * ===Complexity:
     *   - Time: Θ(p * (n + r)) = Θ(n) - where p = 4 (passes per 32-bit int), and r = 256 (digit range)
     *   - Space: Θ(n + r) = Θ(n) - note: pass (4) is sequential, so no accumulation.
     */
    private[LinearSort] def sortInt(numbers: Seq[Int]): Seq[Int] =
      @tailrec
      def loop(elems: Array[Int], exp: Int): Array[Int] =
        if exp > 24 then elems
        else loop(CountingSort.byDigit(elems, exp), exp + 8)

      val unsigned = numbers.toArray.map(_ ^ Int.MinValue)
      loop(unsigned, exp = 0).map(_ ^ Int.MinValue).toIndexedSeq

    /**
     * 64-bit integers radix sort.
     *
     * Handles negative numbers by providing a bijective mapping between signed [-2^63, 2^63) and unsigned [0, 2^64) ints by
     * flipping sign bit with `XOR Long.MinValue`.
     *
     * ===Complexity:
     *   - Time: Θ(p * (n + r)) = Θ(n) - where p = 8 (passes per 64-bit long), and r = 256 (digit range)
     *   - Space: Θ(n + r) = Θ(n) - note: pass (8) is sequential, so no accumulation.
     */
    private[LinearSort] def sortLong(numbers: Seq[Long]): Seq[Long] =
      @tailrec
      def loop(elems: Array[Long], exp: Int): Array[Long] =
        if exp > 56 then elems
        else loop(CountingSort.byDigit(elems, exp), exp + 8)

      val unsigned = numbers.toArray.map(_ ^ Long.MinValue)
      loop(unsigned, exp = 0).map(_ ^ Long.MinValue).toIndexedSeq

    private[LinearSort] def sortByte(numbers: Seq[Byte]) = sortInt(numbers.map(_.toInt)).map(_.toByte)

    private[LinearSort] def sortShort(numbers: Seq[Short]) = sortInt(numbers.map(_.toInt)).map(_.toShort)

    /**
     * A generic radix sort intended for arbitrary-precision numbers (e.g. BigInt) where overflow cannot occur.
     *
     * For **fixed-size types (Char, Byte, Short, Int and Long)**, intermediate `max - min` arithmetic may overflow.
     * **Use the specialised version (e.g. sortByte) instead**.
     *
     * Handles negative numbers with a bijective mapping between [min, max] and [0, k),
     * where `min` is the minimum element in the sequence, `max` is the maximum, and `k` = max - min + 1.
     *
     * Each pass applies a stable counting sort in one digit, starting from the least-significant.
     *
     * ===Complexity:
     *   - Time: Θ(d(n + k)) = Θ(dn) - where d = max number of digits, n = size of `numbers`, and k = 10 (base)
     *   - Space: Θ(n + k)
     *
     * Note:
     *   - d = Θ(log_base M) - where M = max - min
     *   - The larger the base, the fewer the number of passes, but the greater the memory-usage (larger count array).
     */
    private[LinearSort] def sortIntegral[T: ClassTag](numbers: Seq[T])(using num: Integral[T]): Seq[T] =
      import num.*

      @tailrec
      def countDigits(num: T, base: T, acc: Int = 1): Int =
        val r = num.abs / base
        if r > fromInt(0) then countDigits(r, base, acc + 1) else acc

      @tailrec
      def loop(numbers: Array[T], exp: T, base: T, counter: Int): Array[T] =
        val r = CountingSort.byDigit(numbers, exp, base)
        val newCounter = counter - 1
        if newCounter <= 0 then r else loop(r, exp * base, base, newCounter)

      if numbers.size <= 1 then numbers
      else
        val base = fromInt(10)
        val exp = fromInt(1)
        val (min, max) = findMinMax(numbers) // saves a O(n) pass at the cost of complicating maxNumDigits a little.
        val shifted = numbers.map(_ - min)
        val maxNumDigits = countDigits(max - min, base) // needs to shift the original max as well
        loop(shifted.toArray, exp, base, maxNumDigits).map(_ + min).toIndexedSeq

  object CountingSort:

    /**
     * Sorts a well-known dense range of discrete numbers in linear time.
     *
     * Stable: Yes. In-Place: No.
     *
     * ===Algorithm===
     * Let `input` be a sequence of discrete numbers, `min` be the minimum number in the sequence, and `max` be the maximum
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

      if numbers.size <= 1 then numbers
      else
        val (min, max) = findMinMax(numbers)
        val range = max.toLong - min.toLong + 1
        require(range < Int.MaxValue, s"range must be < Int.MaxValue")

        val k = range.toInt

        val count = Array.ofDim[Int](k)
        numbers.foreach { elem =>
          count((elem - min).toInt) += 1
        }

        for i <- 1 until k do count(i) = count(i) + count(i - 1)

        val sorted = Array.ofDim[T](numbers.size) // Uninitialised array saves an O(n) pass
        numbers.reverse.foreach { elem =>
          val index = count((elem - min).toInt).toInt - 1
          sorted(index) = elem
          count((elem - min).toInt) -= 1
        }
        sorted.toIndexedSeq // Based on immutable.ArraySeq.unsafeWrapArray, which is just a wrapper O(1)

    /**
     * Stable counting-sort of fixed-size 32-bit integers by the byte located at the `exp` offset.
     *
     * Assumes all `numbers` are non-negative.
     *
     * ===Complexity===
     *   - Time: Θ(n + 256) = Θ(n)
     *   - Space: Θ(n + 256) = Θ(n)
     */
    private[LinearSort] def byDigit(numbers: Array[Int], exp: Int): Array[Int] =
      require(exp == 0 || exp == 8 || exp == 16 || exp == 24)

      // Consider process 2-bytes at a time (base 65536): 2x faster and (possibly?) L2 cache friendly.
      val RADIX = 256 // To process a 32-bit integer 8 bits at a time (2^8 = 256)
      val MASK = 0xff // To isolate the lowest 8 bytes

      val count = Array.ofDim[Int](RADIX)
      numbers.foreach { elem =>
        val digit = (elem >>> exp) & MASK
        count(digit) += 1
      }

      for i <- 1 until RADIX do count(i) = count(i - 1) + count(i)

      val sorted = Array.ofDim[Int](numbers.size)
      numbers.reverse.foreach { elem =>
        val digit = (elem >>> exp) & MASK
        val index = count(digit) - 1
        sorted(index) = elem
        count(digit) -= 1
      }
      sorted

    /**
     * Stable counting-sort of fixed-size 64-bit integers by the byte located at the `exp` offset.
     *
     * Assumes all `numbers` are non-negative.
     *
     * ===Complexity===
     *   - Time: Θ(n + 256) = Θ(n)
     *   - Space: Θ(n + 256) = Θ(n)
     */
    private[LinearSort] def byDigit(numbers: Array[Long], exp: Int): Array[Long] =
      require(exp >= 0 && exp <= 56 && exp % 8 == 0)

      val RADIX = 256
      val MASK = 0xffL

      val count = Array.ofDim[Int](RADIX)
      numbers.foreach { elem =>
        val digit = (elem >>> exp) & MASK
        count(digit.toInt) += 1
      }

      for i <- 1 until RADIX do count(i) = count(i - 1) + count(i)

      val sorted = Array.ofDim[Long](numbers.size)
      numbers.reverse.foreach { elem =>
        val digit = (elem >>> exp) & MASK
        val index = (count(digit.toInt)) - 1
        sorted(index) = elem
        count(digit.toInt) -= 1
      }
      sorted

    /**
     * Sorts `numbers` with `base` by digit at `exp`. Assumes all `numbers` are non-negative.
     *
     * ===Complexity===
     *   - Time: Θ(n + k) - where n = size of `numbers`, and k = `base`
     *   - Space: Θ(n + k)
     */
    private[LinearSort] def byDigit[T: ClassTag](numbers: Array[T], exp: T, base: T)(using num: Integral[T]): Array[T] =
      import num.*

      require(exp > fromInt(0))
      require(base > fromInt(1))

      def digitFrom(number: T): T = (number / exp) % base

      val k = base.toInt
      val count = Array.ofDim[Int](k)
      numbers.foreach { elem =>
        count(digitFrom(elem).toInt) += 1
      }

      for i <- 1 until k do count(i) = count(i - 1) + count(i)

      val sorted = Array.ofDim[T](numbers.size)
      // note: using while instead of reverse would avoid creating a new array and an additional O(n) allocation.
      numbers.reverse.foreach { elem =>
        val digit = digitFrom(elem)
        val index = count(digit.toInt).toInt - 1
        sorted(index) = elem
        count(digit.toInt) -= 1
      }
      sorted

private def findMinMax[T](numbers: Seq[T])(using num: Integral[T]): (T, T) =
  numbers.foldLeft((numbers.head, numbers.head)) { case (minValue, maxValue) -> elem =>
    (num.min(minValue, elem), num.max(maxValue, elem))
  }
