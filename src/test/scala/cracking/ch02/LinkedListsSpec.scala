package cracking.ch02

import cracking.ch02.LinkedLists._
import org.scalatest.{ FlatSpec, Matchers }

class LinkedListsSpec extends FlatSpec with Matchers {

  "removeDups" should "remove any duplications from list" in {
    removeDups(List(1, 1, 2, 2, 3, 4, 3, 1)) shouldBe List(1, 2, 3, 4)
    removeDups(List(2, 3)) shouldBe List(2, 3)
    removeDups(List.empty[Int]) shouldBe empty

    removeDupsWithBuffer(List(1, 1, 2, 2, 3, 4, 3, 1)) shouldBe List(1, 2, 3, 4)
    removeDupsWithBuffer(List(2, 3)) shouldBe List(2, 3)
    removeDupsWithBuffer(List.empty[Int]) shouldBe empty
  }

  "kthToLast" should "return a list with kth to the last element" in {
    kthToLast(4, List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) shouldBe List(7, 8, 9, 10)
    kthToLast(0, List(1, 2, 3, 4, 5, 6, 7)) shouldBe empty
    kthToLast(3, List.empty) shouldBe empty

    kthToLast2(4, List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) shouldBe List(7, 8, 9, 10)
    kthToLast2(0, List(1, 2, 3, 4, 5, 6, 7)) shouldBe empty
    kthToLast2(3, List.empty) shouldBe empty
  }

  // note that the behaviour here is slightest different to the one in the book
  "removeNode" should "remove element e from the list" in {
    removeNode(9, List(1, 9, 7)) shouldBe List(1, 7)
    removeNode(2, List(1, 9, 7)) shouldBe List(1, 9, 7)
    removeNode(2, List.empty) shouldBe empty
  }

  "partition" should "put all elements less than e before all elements greater or equal x" ignore {
    val aList = List(3, 5, 8, 5, 10, 2, 1)

    val result = partition(5, aList)

    result shouldBe List(3, 2, 1, 5, 8, 5, 10)
    aList.startsWith(result.filter(_ < 5)) shouldBe true
  }
}
