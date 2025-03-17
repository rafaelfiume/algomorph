package cracking.ch02

import cracking.ch02.LinkedLists.*
import munit.Assertions.*
import munit.FunSuite

class LinkedListsSpec extends FunSuite:

  test("remove duplications from a list") {
    assertEquals(removeDups(List(1, 1, 2, 2, 3, 4, 3, 1)), List(1, 2, 3, 4))
    assertEquals(removeDups(List(2, 3)), List(2, 3))
    assertEquals(removeDups(List.empty[Int]), Nil)

    assertEquals(removeDupsWithBuffer(List(1, 1, 2, 2, 3, 4, 3, 1)), List(1, 2, 3, 4))
    assertEquals(removeDupsWithBuffer(List(2, 3)), List(2, 3))
    assertEquals(removeDupsWithBuffer(List.empty[Int]), Nil)
  }

  test("return a list with kth to the last element") {
    assertEquals(kthToLast(4, List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)), List(7, 8, 9, 10))
    assertEquals(kthToLast(0, List(1, 2, 3, 4, 5, 6, 7)), Nil)
    assertEquals(kthToLast(3, List.empty), Nil)

    assertEquals(kthToLast2(4, List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)), List(7, 8, 9, 10))
    assertEquals(kthToLast2(0, List(1, 2, 3, 4, 5, 6, 7)), Nil)
    assertEquals(kthToLast2(3, List.empty), Nil)
  }

  // note that the behaviour here is slightly different from the one in the book
  test("remove element from a list") {
    assertEquals(removeNode(9, List(1, 9, 7)), List(1, 7))
    assertEquals(removeNode(2, List(1, 9, 7)), List(1, 9, 7))
    assertEquals(removeNode(2, List.empty), Nil)
  }

  test("places all elements less than `x` before all elements greater than or equal to `x`") {
    val aList = List(3, 5, 8, 5, 10, 2, 1)

    val result = partition(5, aList)

    assertEquals(result, List(3, 2, 1, 5, 8, 5, 10))
  }
