package fp.ds

import fp.ds.Lists.*
import munit.Assertions.*
import munit.FunSuite

class ListsSpec extends FunSuite:

  test("frequency returns the number of times a word appears in a list") {
    assertEquals(
      frequency(List("this", "is", "a", "that", "is", "a")),
      Map("is" -> 2, "that" -> 1, "a" -> 2, "this" -> 1)
    )
  }

  test("set element e in the n position in a list (0 based)") {
    assertEquals(setElem(List(1, 2, 3, 4, 5), -1, 8), List(1, 2, 3, 4, 5))
    assertEquals(setElem(List(1, 2, 3, 4, 5), 1, 8), List(1, 8, 3, 4, 5))
  }
