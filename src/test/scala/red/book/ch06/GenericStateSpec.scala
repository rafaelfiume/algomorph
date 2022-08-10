package red.book.ch06

import munit.Assertions.*
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*
import red.book.ch06.Machine.simulateMachine
import red.book.ch06.RNG.Simple
import red.book.ch06.RngState.*

class GenericStateSpec extends ScalaCheckSuite:

  property("double generates the next double between 0 and 1") {
    forAll { (seed: Long) =>
      val rng = Simple(seed)

      val (result, _) = double.run(rng)

      assert(result >= 0d && result < 1d)
    }
  }

  property("doubleViaMap generates the next double between 0 and 1") {
    forAll { (seed: Long) =>
      val rng = Simple(seed)

      val result = _double.run(rng)._1

      assert(result >= 0d && result < 1d)
    }
  }

  property("intDouble generate a pair of an int and a double") {
    forAll { (seed: Long) =>
      val rng = Simple(seed)

      val ((rInt, rDouble), _) = intDouble.run(rng)

      assert(rInt >= Int.MinValue && rInt <= Int.MaxValue)
      assert(rDouble >= 0d && rDouble < 1d)
    }
  }

  property("ints (via sequence) generates a list of random integers") {
    forAll { (seed: Long, size: Int) =>
      (size >= 0 && size < 100) ==> {
        val rng = Simple(seed)

        val (result, _) = ints(size).run(rng)

        result.size == size
      }
    }
  }

  property("nonNegativeLessThan generate random integers between 0 (inclusive) and n (exclusive)") {
    forAll { (seed: Long, noLessThan: Int) =>
      (noLessThan > 0) ==> {
        val rng = Simple(seed)

        val (result, _) = RngState.nonNegativeLessThan(noLessThan).run(rng)

        result >= 0d && result < noLessThan
      }
    }
  }

  // Machine

  test("Inserting a coin and turning the knob dispenses a candy") {
    val m = Machine(unlocked = false, candies = 3, coins = 0)

    val ((coins: Int, candies: Int), _) = simulateMachine(List(Coin, Turn)).run(m)

    assertEquals(coins, 1)
    assertEquals(candies, 2)
  }

  test("Turning a knob on a locked machine does nothing") {
    val m = Machine(unlocked = false, candies = 3, coins = 3)

    val ((coins: Int, candies: Int), _) = simulateMachine(List(Turn, Turn, Turn)).run(m)

    assertEquals(coins, 3)
    assertEquals(candies, 3)
  }

  test("Inserting a coin in an unlocked machine does nothing") {
    val m = Machine(unlocked = true, candies = 3, coins = 1)

    val ((coins: Int, candies: Int), _) = simulateMachine(List(Coin, Coin)).run(m)

    assertEquals(coins, 1)
    assertEquals(candies, 3)
  }

  test("A machine that is out of candies ignores all inputs") {
    val m = Machine(unlocked = false, candies = 0, coins = 3)

    val ((coins: Int, candies: Int), _) = simulateMachine(List(Coin, Turn)).run(m)

    assertEquals(coins, 3)
    assertEquals(candies, 0)
  }
