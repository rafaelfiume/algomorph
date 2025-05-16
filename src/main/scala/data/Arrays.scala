package data

import scala.collection.mutable
import scala.util.boundary, boundary.break

object Arrays:

  /*
   * 90-degress clockwise rotation of an NxN matrix.
   *
   * Use this function for reference or if there are no space constraints.
   *
   * Time complexity: n * n = O(n^2).
   * Space complexity: O(n^2).
   */
  def rotate(matrix: Array[Array[Int]]): Array[Array[Int]] =
    val n = matrix(0).size
    require(matrix.forall(_.size == n), "matrix must be NxN")

    val output: Array[Array[Int]] = Array.ofDim(n, n)
    for j <- 0 until n do for i <- 0 until n do output(j)(n - 1 - i) = matrix(i)(j)
    output

  /*
   * In-place 90-degress clockwise rotation of an NxN matrix.
   *
   * Use this function if memory space is restricted, for example, if the matrix is very large.
   *
   * As a trade off for changing the matrix in-place and achieving O(1) space complexity,
   * it will mutate the array passed as input.
   *
   * Time complexity: n * n = O(n^2).
   * Space complexity: O(1).
   */
  def rotateInPlace(matrix: Array[Array[Int]]): Unit =
    val n = matrix(0).size
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

  /*
   * Time complexity: O(n), where n is the size of string.
   */
  def compress(s: String): String =
    var counter = 0
    val compressed = StringBuilder()
    for i <- 0 until s.size do
      counter += 1
      if i + 1 >= s.size || s(i) != s(i + 1) then
        compressed.append(s(i)).append(counter)
        counter = 0
    if compressed.length() > s.size then s else compressed.toString()

  /*
   * Time complexity is O(s) where s is the size of the smallest string.
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
   * Time complexity: O(|s1| + |s2|)
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
        case None =>
          isNegative = true
          Some(-1)
      }: Unit
      for c <- s1 do increaseFrequency(c)
      boundary:
        for c <- s2 do
          decreaseFrequency(c)
          if isNegative then break(false)
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
