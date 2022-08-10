package fp.ds.backtrack

import fp.ds.backtracking.Coins.changeCombs
import munit.Assertions.*
import munit.FunSuite

class CoinsSpec extends FunSuite:

  test("changeCombs returns all the valid combination of coins so that a given amount is made") {
    assertEquals(
      changeCombs(coins = List(5, 2), amount = 16),
      List(List(2, 2, 2, 5, 5), List(2, 2, 2, 2, 2, 2, 2, 2))
    )
  }
