package data

import munit.FunSuite
import scala.concurrent.Future
import munit.GenericBeforeEach
import data.Arrays.*

class ArraysSpec extends FunSuite:

  var m: Array[Array[Int]] = null
  var expectedRotated: Array[Array[Int]] = null

  test("finds peak in an 1D array"):
    assertEquals(findPeak(Array(4, 3, 2, 9)), expected = 0) // 0th index
    assertEquals(findPeak(Array(1, 2, 3, 4, 5, 6, 5, 4)), expected = 5) // 5th index
    assertEquals(findPeak(Array(1)), expected = 0)

  test("finds peak in a 2D array"):
    assertEquals(findPeak(m), expected = (2, 2))
    assertEquals(findPeak(Array(Array(4, 3, 2, 9))), expected = (0, 0)) // 1 row
    assertEquals(findPeak(Array(Array(4), Array(3), Array(2), Array(9))), expected = (3, 0)) // 1 column

  test("nullifies row and colunm of an element when it is set to zero"):
    m(0)(1) = 0

    nullifyInPlace(m)

    assert(0 == m(0)(0)); assert(m(0)(0) == m(0)(1)); assert(m(0)(1) == m(0)(2)) // first row nullified
    assert(0 == m(1)(1)); assert(m(0)(1) == m(1)(1)); assert(m(1)(1) == m(2)(1)) // second column nullified
    assertEquals(m(1)(0), 4); assertEquals(m(1)(2), 6);
    assertEquals(m(2)(0), 7); assertEquals(m(2)(2), 9);

  test("rotates a NxN matrix"):
    val result = rotate(m)
    assertEquals(result(0).toSeq, expectedRotated(0).toSeq)
    assertEquals(result(1).toSeq, expectedRotated(1).toSeq)
    assertEquals(result(2).toSeq, expectedRotated(2).toSeq)

  test("rotates in place a NxN matrix"):
    rotateInPlace(m)
    assertEquals(m(0).toSeq, expectedRotated(0).toSeq)
    assertEquals(m(1).toSeq, expectedRotated(1).toSeq)
    assertEquals(m(2).toSeq, expectedRotated(2).toSeq)

  test("finds smallest missing positive"):
    assertEquals(findSmallestMissingPositive(Array(5, 3, 9, 1, 4, 7, 2)), 6)
    assertEquals(findSmallestMissingPositive(Array(1, 2)), 3)
    assertEquals(findSmallestMissingPositive(Array(-111, -33)), 1)
    assertEquals(findSmallestMissingPositive(Array(1)), 2)
    assertEquals(findSmallestMissingPositive(Array(2)), 1)
    assertEquals(findSmallestMissingPositive(Array.empty[Int]), -1)

  test("checks if rotation"):
    assert(isRotation("waterbottle", "erbottlewat"))
    assert(!isRotation("waterbottl3", "erbottlewat"))

  test("compresses a string"):
    assertEquals(compress("aabcccccaaa"), "a2b1c5a3")
    assertEquals(compress(""), "")
    assertEquals(compress("abc"), "abc")

  test("checks if a string is at most one edit away from another"):
    assert(isAtMostOneAway("pale", "pale")) // no edits
    assert(isAtMostOneAway("pale", "bale")) // one edit
    assert(!isAtMostOneAway("pale", "bake")) // two edits
    assert(isAtMostOneAway("pale", "ple")) // one removal
    assert(isAtMostOneAway("pales", "pale")) // one insertion
    assert(!isAtMostOneAway("paless", "pale")) // two insertions
    // edge cases
    assert(isAtMostOneAway("", ""))
    assert(isAtMostOneAway("a", ""))
    assert(isAtMostOneAway("", "a"))

  override def beforeEach(context: GenericBeforeEach[Future[Any]]): Unit =
    m = Array.ofDim(3, 3)
    m(0)(0) = 1; m(0)(1) = 2; m(0)(2) = 3
    m(1)(0) = 4; m(1)(1) = 5; m(1)(2) = 6
    m(2)(0) = 7; m(2)(1) = 8; m(2)(2) = 9

    expectedRotated = Array.ofDim(3, 3)
    expectedRotated(0)(0) = 7; expectedRotated(0)(1) = 4; expectedRotated(0)(2) = 1
    expectedRotated(1)(0) = 8; expectedRotated(1)(1) = 5; expectedRotated(1)(2) = 2
    expectedRotated(2)(0) = 9; expectedRotated(2)(1) = 6; expectedRotated(2)(2) = 3
