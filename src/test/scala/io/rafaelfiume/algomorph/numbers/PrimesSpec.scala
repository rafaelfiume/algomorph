package io.rafaelfiume.algomorph.numbers

import io.rafaelfiume.algomorph.numbers.Primes.*
import io.rafaelfiume.algomorph.numbers.Primes.Reference.isPrime
import munit.{FunSuite, ScalaCheckSuite}
import org.scalacheck.Gen.*
import org.scalacheck.Prop.*
import org.scalacheck.ShrinkLowPriority

class PrimesSpec extends ScalaCheckSuite with ShrinkLowPriority:

  test("checks if prime"):
    for p <- smallPrimes do assert(isPrime(p), s"$p is prime")

  test("checks if composite"):
    for c <- smallComposites do assert(!isPrime(c), s"$c is not prime")

  test("generates primes via Sieve of Eratosthenes returns"):
    assertEquals(primes(2), Vector(2))
    assertEquals(primes(30), expected = Vector(2, 3, 5, 7, 11, 13, 17, 19, 23, 29))
    assertEquals(primes(100).length, expected = 25) // 25 primes < 100
    assert(primes(1000).forall { prime => (2 until prime).forall(prime % _ != 0) }) // no composites

  test("sieve is stack-safe"):
    // completes in around 1.2s! `sives` is many order of magnitude faster than `primes` (see above)
    assertEquals(primes(10_000_000).length, expected = 664_579)

  test("factorise decomposites numbers into prime factors"):
    assertEquals(factorise(1), expected = Map.empty)
    assertEquals(factorise(2), expected = Map(BigInt(2) -> 1))
    assertEquals(factorise(17), expected = Map(BigInt(17) -> 1))
    assertEquals(factorise(75), expected = Map(BigInt(3) -> 1, BigInt(5) -> 2))
    assertEquals(factorise(30), expected = Map(BigInt(2) -> 1, BigInt(3) -> 1, BigInt(5) -> 1))
    assertEquals(factorise(8), expected = Map(BigInt(2) -> 3))

  /*
   * Reference implementations:
   */

  val smallPrimes = List(2, 3, 5, 7, 11, 13, 17, 19)
  val smallComposites = List(4, 6, 8, 9, 10, 12, 14, 15, 16, 18)

  val maxVal = 5000

  test("ref. impl. checks if prime"):
    for p <- smallPrimes do assert(Reference.isPrime(p), s"$p is prime")

  test("ref. impl. checks if composite"):
    for c <- smallComposites do assert(!Reference.isPrime(c), s"$c is not prime")

  property("fast vs reference isPrime agree"):
    forAll(choose(2, 5_000_000)) { n =>
      isPrime(n) == Reference.isPrime(n)
    }

  test("ref. impl. generates primes"):
    assertEquals(Reference.primes(999), 7919)
    assertEquals(Reference.primes.take(10).toList, expected = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29))

  property("fast vs reference primes agree"):
    forAll(choose(2, maxVal)) { n =>
      primes(n) == Reference.primes.takeWhile(_ <= n).toVector
    }

  // the test succeeds, but it takes 106m to return the number of primes < 10M.
  test("ref. impl. primes is stack-safe".ignore):
    // Prime Number Theory: number of primes not exceeding n is roughly n/ln n.
    // Let n = 10,000,000, then π(n) = 620,420. The actual number is 664,579.
    assertEquals(Reference.primes.takeWhile(_ <= 10_000_000).length, expected = 664_579)

  test("ref. impl. gcd returns the greatest common divisor between two positive integers"):
    assertEquals(Reference.gcd(24, 36).toInt, expected = 12)
    assertEquals(Reference.gcd(120, 500).toInt, expected = 20)
    assertEquals(Reference.gcd(17, 22).toInt, expected = 1)

  test("ref. impl. lcm returns the lowest common multile between two positive intergers"):
    assertEquals(Reference.lcm(4, 5).toInt, expected = 20)
    assertEquals(Reference.lcm(60, 90).toInt, expected = 180)
