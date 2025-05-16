package data

import munit.FunSuite

class ArraysSpec extends FunSuite:

  val m: Array[Array[Int]] = Array.ofDim(3, 3)
  m(0)(0) = 1; m(0)(1) = 2; m(0)(2) = 3
  m(1)(0) = 4; m(1)(1) = 5; m(1)(2) = 6
  m(2)(0) = 7; m(2)(1) = 8; m(2)(2) = 9

  val expected: Array[Array[Int]] = Array.ofDim(3, 3)
  expected(0)(0) = 7; expected(0)(1) = 4; expected(0)(2) = 1
  expected(1)(0) = 8; expected(1)(1) = 5; expected(1)(2) = 2
  expected(2)(0) = 9; expected(2)(1) = 6; expected(2)(2) = 3

  test("rotates a NxN matrix"):
    val result = Arrays.rotate(m)
    assertEquals(result(0).toSeq, expected(0).toSeq)
    assertEquals(result(1).toSeq, expected(1).toSeq)
    assertEquals(result(2).toSeq, expected(2).toSeq)

  test("rotates in place a NxN matrix"):
    Arrays.rotateInPlace(m)
    assertEquals(m(0).toSeq, expected(0).toSeq)
    assertEquals(m(1).toSeq, expected(1).toSeq)
    assertEquals(m(2).toSeq, expected(2).toSeq)

  test("compresses a string"):
    assertEquals(Arrays.compress("aabcccccaaa"), "a2b1c5a3")
    assertEquals(Arrays.compress(""), "")
    assertEquals(Arrays.compress("abc"), "abc")

  test("checks if a string is at most one edit away from another"):
    assert(Arrays.isAtMostOneAway("pale", "pale")) // no edits
    assert(Arrays.isAtMostOneAway("pale", "bale")) // one edit
    assert(!Arrays.isAtMostOneAway("pale", "bake")) // two edits
    assert(Arrays.isAtMostOneAway("pale", "ple")) // one removal
    assert(Arrays.isAtMostOneAway("pales", "pale")) // one insertion
    assert(!Arrays.isAtMostOneAway("paless", "pale")) // two insertions
    // edge cases
    assert(Arrays.isAtMostOneAway("", ""))
    assert(Arrays.isAtMostOneAway("a", ""))
    assert(Arrays.isAtMostOneAway("", "a"))
