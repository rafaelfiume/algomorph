package numbers

import munit.FunSuite
import numbers.BinomialProbability.*
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.*
import org.scalacheck.Prop

class BinomialProbabilitySpec extends ScalaCheckSuite:

  test("calculates factorial"):
    assertEquals(factorial(0).toInt, expected = 1)
    assertEquals(factorial(1).toInt, expected = 1)
    assertEquals(factorial(10).toInt, expected = 3628800)

  test("calculates binomial coefficient"):
    assertEquals(binomialCoefficient(3, 2).toInt, expected = 3)
    assertEquals(binomialCoefficient(4, 1).toInt, expected = 4)
    assertEquals(binomialCoefficient(4, 3).toInt, expected = 4)
    assertEquals(binomialCoefficient(5, 0).toInt, expected = 1)
    assertEquals(binomialCoefficient(5, 2).toInt, expected = 10)
    assertEquals(binomialCoefficient(5, 3).toInt, expected = 10)
    assertEquals(binomialCoefficient(5, 5).toInt, expected = 1)
    assertEquals(binomialCoefficient(50, 3).toDouble, expected = math.pow(140, 2))
    assertEquals(binomialCoefficient(1_000_000, 999_999).toInt, 1_000_000) // C(n, k) = C(n, n-k)

  test("probability of a specific sequence with k successes and n-k failures, with success probability p = 0.6 per attempt"):
    assertEquals(exactlyIndependentEventsInSequence(n = 1, k = 1, p = 0.6), expected = 0.6)
    assertEquals(exactlyIndependentEventsInSequence(n = 3, k = 2, p = 0.6), expected = 0.144)

  test("probability of exactly k successes out of n attempts"):
    assertEquals(exactlytKSuccesses(n = 1, k = 1, p = 0.6), expected = 0.6) // one success
    assertEquals(exactlytKSuccesses(3, 3, 0.6), expected = math.pow(0.6, 3)) // all successes
    assertEquals(exactlytKSuccesses(n = 3, k = 0, 0.6), expected = math.pow(0.4, 3)) // all failures
    assertEqualsDouble(exactlytKSuccesses(n = 3, k = 2, p = 0.6), expected = 0.432, delta = 0.000000001)

  test("probability of at least k successes out of n attempts"):
    assertEquals(atLeastKSuccesses(n = 1, k = 1, p = 0.6), expected = 0.6)
    assertEqualsDouble(atLeastKSuccesses(n = 3, k = 2, p = 0.6), expected = 0.648, delta = 0.000000001)

  property("probability theory to select optimal game") {
    forAll(Gen.choose(0.0, 1.0)) { p =>
      // Game 1 (1/1 success): favours beginners - fewer chances for mistakes
      val game1 = exactlytKSuccesses(n = 1, k = 1, p)
      // Game 2 (>=2/3 successes): favours experts, since more attempts reduce variance (Law of Large Numbers)
      val game2 = atLeastKSuccesses(n = 3, k = 2, p)

      /* Optimal choice: Game 1 vs Game 2  => P1 = p; P2 = 3p^2 - 2p^3
       * Solve: P2 > P1 => 2p^2 - 3p + 1 < 0 ==> Roots at p = 0.5 and p = 1.0
         Decision boundaries: */
      if p < 0.5 then (game1 > game2) :| "Select game 1 if p < 0.5 (beginner to intermediate)"
      else if p > 0.5 then (game1 < game2) :| "Select game 2 if p > 0.5 (intermediate to expert)"
      else (game1 == game2) :| "Select either game 1 or game 2 if p = 0.5 (intermediate)"
    }
  }
