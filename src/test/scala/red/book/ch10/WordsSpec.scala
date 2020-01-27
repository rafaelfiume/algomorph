package red.book.ch10

import org.scalatest.{ FlatSpec, Matchers }

class WordsSpec extends FlatSpec with Matchers {

//  "string" should "be converted to WC" in {
//    WC("lorem") shouldEqual Stub("lorem")
//    WC("lorem ipsum do") shouldEqual Part("lorem", 1, "do")
//    WC("lorem sit amet, ") shouldEqual Part("lorem", 2, "")
//    WC("lorem sit amet, ") shouldEqual Part("lorem", 2, "")
//  }

  "WC" should "be able to count words" in {
    WC.countWords("a casa da vovo e do vovo") shouldEqual 7
    WC.countWords(" lorem ipsum do ") shouldEqual 3
    WC.countWords("a casa da vovo e do vovo this is a not very very long test and I'd like to know how to solve this issue") shouldEqual 25
  }
}
