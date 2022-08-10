package fp.ds.bits

import fp.ds.bits.Bits.*
import munit.Assertions.*
import munit.FunSuite

class BitsSpec extends FunSuite:

  test("add two binary numbers") {
    assertEquals(add(List(1, 1), List.empty), List(1, 1))
    assertEquals(add(List.empty, List(1, 1)), List(1, 1))
    assertEquals(add(List(1), List(1)), List(1, 0))
    assertEquals(add(List(1, 1, 1, 1, 0), List(0, 1, 0, 1, 1)), List(1, 0, 1, 0, 0, 1))
    assertEquals(add(List(1, 1), List(1, 1, 0, 1)), List(1, 0, 0, 0, 0))
  }

  test("carry on an inverted list (left to right, for performance reasons)") {
    assertEquals(carry(1, List(0)), List(1))
    assertEquals(carry(1, List(1)), List(0, 1))
    assertEquals(carry(1, List(1, 1, 0, 1)), List(0, 0, 1, 1))
  }
