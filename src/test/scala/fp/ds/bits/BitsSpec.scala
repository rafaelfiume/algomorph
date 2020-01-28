package fp.ds.bits

import fp.ds.bits.Bits._
import org.scalatest.{ FlatSpec, Matchers }

class BitsSpec extends FlatSpec with Matchers {

  "add" should "add two binary numbers" in {
    add(List(1,1), List.empty) shouldBe List(1,1)
    add(List.empty, List(1,1)) shouldBe List(1,1)
    add(List(1), List(1)) shouldBe List(1,0)
    add(List(1,1,1,1,0),
        List(0,1,0,1,1)) shouldBe List(1,0,1,0,0,1)
    add(List(    1,1),
        List(1,1,0,1)) shouldBe List(1,0,0,0,0)
  }

  "carry" should "perform the carry operation on an inverted list (left to right, for performance reasons)" in {
    carry(1, List(0)) shouldBe List(1)
    carry(1, List(1)) shouldBe List(0,1)
    carry(1, List(1,1,0,1)) shouldBe List(0,0,1,1)
  }
}
