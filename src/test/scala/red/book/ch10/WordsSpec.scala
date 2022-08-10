package red.book.ch10

import munit.Assertions.*
import munit.FunSuite

class WordsSpec extends FunSuite:

//  "string" should "be converted to WC") {
//    WC("lorem") shouldEqual Stub("lorem")
//    WC("lorem ipsum do") shouldEqual Part("lorem", 1, "do")
//    WC("lorem sit amet, ") shouldEqual Part("lorem", 2, "")
//    WC("lorem sit amet, ") shouldEqual Part("lorem", 2, "")
//  }

  test("WC counts words") {
    assertEquals(WC.countWords("a casa da vovo e do vovo"), 7)
    assertEquals(WC.countWords(" lorem ipsum do "), 3)
    assertEquals(
      WC.countWords("a casa da vovo e do vovo this is a not very very long test and I'd like to know how to solve this issue"),
      25
    )
  }
