package fp.ds

import fp.ds.Lists._
import org.scalatest.{ FlatSpec, Matchers }

class ListsSpec extends FlatSpec with Matchers {

  "frequency" should "tell the number of times a word appears in a list" in {
    frequency(List("this", "is", "a", "that", "is", "a")) should contain theSameElementsAs Map("is" -> 2, "that" -> 1, "a" -> 2, "this" -> 1)
  }

  "setElem" should "set element e in the n position in a list (0 based)" in {
    setElem(List(1,2,3,4,5), 1, 8) shouldBe List(1,8,3,4,5)

    setElem(List(1,2,3,4,5), -1, 8) shouldBe List(1,2,3,4,5)
  }
}
