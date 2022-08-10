package fp.ds.greedy

import Coins.change
import munit.Assertions.*
import munit.FunSuite

class CoinsSpec extends FunSuite:

  test("change selects coins so a given amount is made") {
    assertEquals(change(coins = List(7, 2), amount = 16), List(2, 7, 7))
    assertEquals(change(coins = List(5, 2), amount = 16), List(5, 5, 5)) // gives the suboptimal solution!!!!!
  }
