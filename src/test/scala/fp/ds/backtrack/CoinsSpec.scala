package fp.ds.backtrack

import fp.ds.backtracking.Coins.changeCombs
import org.scalatest.{ FlatSpec, Matchers }

class CoinsSpec extends FlatSpec with Matchers {

  "changeCombs" should "return all the valid combination of coins so that a given amount is made" in {
    changeCombs(coins = List(5,2), amount = 16) shouldBe List(List(2, 2, 2, 5, 5), List(2, 2, 2, 2, 2, 2, 2, 2))
  }
}
