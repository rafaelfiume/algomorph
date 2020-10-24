package red.book.ch06

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import red.book.ch06.Machine.simulateMachine
import red.book.ch06.RNG.Simple
import red.book.ch06.RngState._

class GenericStateSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  "double" should "generate the next double between 0 and 1" in {
    forAll { seed: Long =>
      val rng = Simple(seed)

      val (result, _) = double.run(rng)

      result should (be >= 0d and be < 1d)
    }
  }

  "doubleViaMap" should "generate the next double between 0 and 1" in {
    forAll { seed: Long =>
      val rng = Simple(seed)

      val result = _double.run(rng)._1

      result should (be >= 0d and be < 1d)
    }
  }

  "intDouble" should "generate a pair of an int and a double " in {
    forAll { seed: Long =>
      val rng = Simple(seed)

      val ((rInt, rDouble), _) = intDouble.run(rng)

      rInt should (be >= Int.MinValue and be <= Int.MaxValue)
      rDouble should (be >= 0d and be < 1d)
    }
  }

  "ints (via sequence)" should "generate a list of random integers" in {
    forAll { (seed: Long, size: Int) =>
      whenever (size >= 0 && size < 100) {
        val rng = Simple(seed)

        val (result, _) = ints(size).run(rng)

        result.size shouldBe size
      }
    }
  }

  "nonNegativeLessThan" should "generate random integers between 0 (inclusive) and n (exclusive)" in {
    forAll { (seed: Long, noLessThan: Int) =>
      whenever(noLessThan > 0) {
        val rng = Simple(seed)

        val (result, _) = RngState.nonNegativeLessThan(noLessThan).run(rng)

        result should (be >= 0 and be < noLessThan)
      }
    }
  }

  // Machine

  "Inserting a coin and turning the knob" should "dispense a candy" in {
    val m = Machine(unlocked = false, candies = 3, coins = 0)

    val ((coins: Int, candies: Int), _) = simulateMachine(List(Coin, Turn)).run(m)

    coins shouldBe 1
    candies shouldBe 2
  }

  "Turning a knob on a locked machine" should "do nothing" in {
    val m = Machine(unlocked = false, candies = 3, coins = 3)

    val ((coins: Int, candies: Int), _) = simulateMachine(List(Turn, Turn, Turn)).run(m)

    coins shouldBe 3
    candies shouldBe 3
  }

  "Inserting a coin in an unlocked machine" should "do nothing" in {
    val m = Machine(unlocked = true, candies = 3, coins = 1)

    val ((coins: Int, candies: Int), _) = simulateMachine(List(Coin, Coin)).run(m)

    coins shouldBe 1
    candies shouldBe 3
  }

  "A machine that is out of candies" should "ignore all inputs" in {
    val m = Machine(unlocked = false, candies = 0, coins = 3)

    val ((coins: Int, candies: Int), _) = simulateMachine(List(Coin, Turn)).run(m)

    coins shouldBe 3
    candies shouldBe 0
  }

}
