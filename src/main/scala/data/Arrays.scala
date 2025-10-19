package data

import Ordering.Implicits.*
import scala.collection.mutable
import scala.util.boundary, boundary.break
import scala.annotation.tailrec
import scala.reflect.ClassTag

object Arrays:

  // TODO Multiplication of matrices with Scala capabilities

  /**
   * 1D peak-finding algorithm.
   *
   * It leverages local monotonicity of a rising slope on either side of the array to ensure progress towards a peak and return
   * it.
   *
   * ===Algorithm===
   *   - Find the mid element A[i]
   *   - Compare mid A[i] with its left A[i-1] and right A[i+1] neighbours
   *   - If left neighbour is greater than mid, recurse left (left..mid-1)
   *   - If right neighbour is greater than i, recurse right (mid+1..right)
   *   - Otherwise, A[i] is the 1D peak.
   *
   * ===Complexity===
   *   - Time: Θ(log n) - where n = length of array
   *   - Space: Θ(log n)
   */
  def findPeak[A: Ordering](array: Array[A]): Int =
    require(array.nonEmpty, "provide a non-empty array")
    @tailrec
    def search(left: Int, right: Int): Int =
      (right - left) match
        case 1 => left
        case 2 => if array(left) >= array(right - 1) then left else right - 1
        case _ =>
          val mid = (right + left) / 2
          if array(mid) >= array(mid - 1) && array(mid) >= array(mid + 1) then mid
          else if array(mid - 1) > array(mid) then search(left, mid)
          else search(mid + 1, right)

    search(0, array.size)

  /**
   * 2D peak-finding algorithm.
   *
   * ===Algorithm===
   *   - Find the mid column j
   *   - Find the global max A[i, j]
   *     - A[i, j] is greater than its top A[i-1, j] and bottom A[i+1, j] neighbours
   *   - Compare A[i, j] with its left A[i, j-1] and right A[i, j+1] neighbours
   *     - If A[i, j-1] > A[i, j], recurse left (columns left...mid-1)
   *     - If A[i, j+1] > A[i, j], recurse right (columns mid+1...right)
   *     - Otherwise, A[i, j] is the 2D peak.
   *
   * ===Complexity===
   * Let n = number of rows, m = number of columns:
   *   - Time: Θ(n log m) - T(m/2) * T(n)
   *   - Space: Θ(log m)
   */
  def findPeak[A: Ordering](matrix: Array[Array[A]]): (Int, Int) =
    require(matrix.nonEmpty && matrix(0).nonEmpty, "provide a non-empty NxM array")
    val numCols = matrix(0).size

    @tailrec
    def search(left: Int, right: Int): (Int, Int) =
      val midCol = (left + right) / 2
      val maxRow = matrix.indices.maxBy(row => matrix(row)(midCol))
      if midCol - 1 >= 0 && matrix(maxRow)(midCol - 1) > matrix(maxRow)(midCol) then search(left, midCol - 1)
      else if midCol + 1 < numCols && matrix(maxRow)(midCol + 1) > matrix(maxRow)(midCol) then search(midCol + 1, right)
      else (maxRow, midCol)

    search(left = 0, right = matrix(0).size)

  /*
   * Nullifies in-place rows and columns of elements of an NxM matrix when set to 0.
   *
   * ===Complexity===
   *   - Time: Θ(NM)
   *   - Space: Θ(1)
   *
   * Performance Notes:
   *   - The best we can come up with is a quadratic time complexity,
   * since all elements of the matrix have to be accessed in order to implement this function.
   */
  def nullifyInPlace(matrix: Array[Array[Int]]): Unit =
    val n = matrix.size
    require(n > 0, "requires a non-empty matrix")
    val m = matrix(0).size
    require(matrix.forall(_.size == m), "matrix must be NxM")

    def findRemainingZeros(): Unit =
      for i <- 1 until n; j <- 1 until m do
        if matrix(i)(j) == 0 then
          matrix(i)(0) = 0
          matrix(0)(j) = 0

    def nullifyRow(i: Int) = for j <- 0 until m do matrix(i)(j) = 0
    def nullifyColumn(j: Int) = for i <- 0 until n do matrix(i)(j) = 0

    val firstRowHasZeros = matrix(0).exists(_ == 0) // O(m)
    val firstColumnHasZeros = matrix.exists(row => row(0) == 0) // O(n)
    findRemainingZeros()

    for i <- 1 until n do if matrix(i)(0) == 0 then nullifyRow(i)
    for j <- 1 until m do if matrix(0)(j) == 0 then nullifyColumn(j)
    if firstRowHasZeros then nullifyRow(0)
    if firstColumnHasZeros then nullifyColumn(0)

  /*
   * 90-degrees clockwise rotation of an NxN matrix.
   *
   * Use this function for reference or if there are no space constraints.
   *
   * ===Complexity===
   *   - Time: Θ(n^2).
   *   - Space: Θ(n^2).
   */
  def rotate(matrix: Array[Array[Int]]): Array[Array[Int]] =
    val n = matrix.size
    require(matrix.forall(_.size == n), "matrix must be NxN")

    val output: Array[Array[Int]] = Array.ofDim(n, n)
    for j <- 0 until n do for i <- 0 until n do output(j)(n - 1 - i) = matrix(i)(j)
    output

  /*
   * In-place 90-degrees clockwise rotation of an NxN matrix.
   *
   * Use this function if memory space is restricted, for example, if the matrix is very large.
   *
   * ===Complexity===
   *   - Time: Θ(n^2)
   *   - Space: Θ(1)
   *
   * Performance Notes:
   *   - As a trade off for changing the matrix in-place and achieving O(1) space complexity,
   * it will mutate the array passed as input.
   */
  def rotateInPlace(matrix: Array[Array[Int]]): Unit =
    val n = matrix.size
    require(matrix.forall(_.size == n), "matrix must be NxN")

    for layer <- 0 until n / 2 do
      val first = layer
      val last = n - 1 - layer
      for i <- first until last do
        val offset = i - first
        val tmp = matrix(first)(i) // tmp <- top
        matrix(first)(i) = matrix(last - offset)(first) // top <- left
        matrix(last - offset)(first) = matrix(last)(last - offset) // left <- bottom
        matrix(last)(last - offset) = matrix(offset)(last) // bottom <- right
        matrix(offset)(last) = tmp // right <- tmp

  /**
   * Find the smallest positive integer not present in the array.
   *
   * ===Evaluation Semantics===
   * `findSmallestMissingPositive(Array(5, 3, 9, 1, 4, 7, 2))` executes as:
   * {{{
   * - Sort: [1, 2, 3, 4, 5, 7, 9]
   * - Find gap between 5 and 7: Return 6
   * }}}
   *
   * ===Real-World Use Cases===
   *   - Database ID allocation (find the next ID available)
   *   - Scheduling systems (find first available time slot)
   *   - Inventory management (find missing item codes)
   *
   * ===Complexity===
   *   - Time: Θ(n log n) - dominated by sorting
   *   - Space: Θ(n) - for the sorted copy
   */
  def findSmallestMissingPositive(a: Array[Int]): Int =
    if a.isEmpty then -1
    else
      val sorted = a.sorted
      var result = 1
      boundary:
        for i <- sorted do
          if result == i then result += 1
          if result < i then break()
      result

  /*
   * Checks if s2 is a rotation of s1.
   *
   * Let s1 = xy, where x and y are substrings. A rotation of s1 would be yx.
   * If s2 is a rotation of s1, then s2 = yx and s1 = x2.
   * s1 + s1 equals xyxy, and s2 is a substring of s2.
   *
   * ===Complexities===
   *   - Time: Θ(n) - bounded by String concatanation (`+``) and `contains`
   */
  def isRotation(s1: String, s2: String): Boolean =
    s1.nonEmpty && s1.size == s2.size && (s1 + s1).contains(s2)

  /*
   * ===Complexity===
   *   - Time: Θ(n) - where n is the size of string
   */
  def compress(s: String): String =
    var counter = 0
    val compressed = StringBuilder()
    for i <- 0 until s.size do
      counter += 1
      if i + 1 >= s.size || s(i) != s(i + 1) then
        val _ = compressed.append(s(i)).append(counter)
        counter = 0
    if compressed.length() > s.size then s else compressed.toString()

  /*
   * ===Complexity===
   *   - Time: Θ(s) - where s is the size of the smallest string
   */
  def isAtMostOneAway(s1: String, s2: String): Boolean =
    def checkReplacements(one: String, another: String): Boolean =
      var hasEdit = false
      boundary:
        for i <- 0 until one.size do
          if one(i) != another(i) && !hasEdit then hasEdit = true
          else if one(i) != another(i) then break(false)
        true

    def checkInsertion(longer: String, shorter: String): Boolean =
      var i = 0
      var j = 0
      boundary:
        while i < longer.size do
          if j < shorter.size && longer(i) == shorter(j) then j += 1
          i += 1
          if i - j > 1 then break(false)
        true

    if (s1.size - s2.size).abs > 1 then false
    else
      (s1.size, s2.size) match
        case (a, b) if a == b => checkReplacements(s1, s2)
        case (a, b) if a > b  => checkInsertion(s1, s2)
        case _                => checkInsertion(s2, s1)

  /*
   * ===Complexity===
   *   - Time: Θ(|s1| + |s2|)
   */
  def isPermutation(s1: String, s2: String): Boolean =
    if s1.size != s2.size then false
    else
      val frequencies = mutable.Map.empty[Char, Int]
      def increaseFrequency(c: Char): Unit = frequencies.updateWith(c) {
        case Some(f) => Some(f + 1)
        case None    => Some(1)
      }: Unit
      var isNegative = false
      def decreaseFrequency(c: Char): Unit = frequencies.updateWith(c) {
        case Some(1) => None
        case Some(f) => Some(f - 1)
        case None    =>
          isNegative = true
          Some(-1)
      }: Unit
      for c <- s1 do increaseFrequency(c)
      boundary:
        for c <- s2 do
          decreaseFrequency(c)
          if isNegative then break()
      !isNegative && frequencies.size == 0

  object Fp:
    def isPermutation(s1: String, s2: String): Boolean =
      if s1.size != s2.size then false
      else
        val frequencies = s1.groupMapReduce(identity)(_ => 1)(_ + _)
        boundary:
          s2.foldLeft(frequencies) { (acc, s) =>
            acc.get(s) match
              case Some(1) => acc - s
              case Some(n) => acc.updated(s, n - 1)
              case None    => break(false)
          }.isEmpty

object PlayStrings:
  import data.Arrays.isPermutation
  import data.Arrays.Fp

  def main(args: Array[String]): Unit =
    println(s"${{ isPermutation("abcde", "ecdba") }}")
    println(s"${{ Fp.isPermutation("abcde", "ecdba") }}")
    // println(s"${{ isPermutation("abcde", "ecdb") }}")
    // println(s"${{ isPermutation("bcde", "ecdba") }}")
    // println(s"${{ isPermutation("aaaa", "bbbb") }}")
    println(s"${{ isPermutation("aaab", "aaac") }}")
    println(s"${{ Fp.isPermutation("aaab", "aaac") }}")
