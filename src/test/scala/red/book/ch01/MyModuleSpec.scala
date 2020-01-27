package red.book.ch01

import red.book.ch01.MyModule.fib
import org.scalatest.{ FlatSpec, Matchers }

class MyModuleSpec extends FlatSpec with Matchers {

  "fib" should "retrieve the nth number in a fibonacy sequence (0,1,1,2,3,5,8..)" in {
//    fib(-1) shouldBe 0 // invalid
//    fib(0) shouldBe 0 // invalid
    fib(1) shouldBe 0
    fib(2) shouldBe 1
    fib(3) shouldBe 1
    fib(4) shouldBe 2
    fib(5) shouldBe 3
    fib(6) shouldBe 5
    fib(7) shouldBe 8
  }
}