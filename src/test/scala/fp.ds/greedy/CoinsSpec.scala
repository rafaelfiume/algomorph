package fp.ds.greedy

import fp.ds.greedy.Coins.change
import org.scalatest.{ FlatSpec, Matchers }

class CoinsSpec extends FlatSpec with Matchers {

  "change" should "select coins so a given amount is made" in {
    change(coins = List(7,2), amount = 16) shouldBe List(2,7,7)
    change(coins = List(5,2), amount = 16) shouldBe List(5,5,5) // gives the suboptimal solution!!!!!
  }
}
