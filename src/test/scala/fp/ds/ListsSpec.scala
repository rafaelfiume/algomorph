package fp.ds

import fp.ds.Lists.*
import munit.Assertions.*
import munit.FunSuite

class ListsSpec extends FunSuite:

  test("return the number of times a word appears in a list") {
    assertEquals(
      frequency(List("this", "is", "a", "that", "is", "a")),
      Map("is" -> 2, "that" -> 1, "a" -> 2, "this" -> 1)
    )
  }

  test("set element `x` at index `n` in a 0-based list") {
    assertEquals(setElem(List(1, 2, 3, 4, 5), -1, 8), List(1, 2, 3, 4, 5))
    assertEquals(setElem(List(1, 2, 3, 4, 5), 1, 8), List(1, 8, 3, 4, 5))
  }
