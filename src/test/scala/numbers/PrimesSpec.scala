package numbers

import numbers.Primes.*
import munit.FunSuite

class PrimesSpec extends FunSuite:

  test("checks if prime"):
    for p <- Set(2, 3, 5, 7, 11, 13, 17, 19) do assert(isPrime(p), s"$p is prime")

  test("checks if composite"):
    for c <- Set(4, 6, 8, 9, 10, 12, 14, 15, 16, 18) do assert(!isPrime(c), s"$c is not prime")

  test("generates primes"):
    assertEquals(primes(999), 7919)
    assertEquals(primes.take(10).toList, expected = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29))

  // the test succeeds, but it takes 106m to return the number of primes < 10M.
  test("primes is stack-safe".ignore):
    // Prime Number Theory: number of primes not exceeding n is roughly n/ln n.
    // Let n = 10,000,000, then π(n) = 620,420. The actual number is 664,579.
    assertEquals(primes.takeWhile(_ <= 10_000_000).length, expected = 664_579)

  test("sieve returns primes"):
    assertEquals(sieve(2), Vector(2))
    assertEquals(sieve(30), expected = Vector(2, 3, 5, 7, 11, 13, 17, 19, 23, 29))
    assertEquals(sieve(100).length, expected = 25) // 25 primes < 100
    assert(sieve(1000).forall { prime => (2 until prime).forall(prime % _ != 0) }) // no composites

  test("sieve is stack-safe"):
    // completes in around 1.2s! `sives` is many order of magnitude faster than `primes` (see above)
    assertEquals(sieve(10_000_000).length, expected = 664_579)

  test("gcd returns the greatest common divisor between two positive integers"):
    assertEquals(gcd(24, 36).toInt, expected = 12)
    assertEquals(gcd(120, 500).toInt, expected = 20)
    assertEquals(gcd(17, 22).toInt, expected = 1)

  test("lcm returns the lowest common multile between two positive intergers"):
    assertEquals(lcm(4, 5).toInt, expected = 20)
    assertEquals(lcm(60, 90).toInt, expected = 180)

  test("factorise decomposites numbers into prime factors"):
    assertEquals(factorise(1), expected = Map.empty)
    assertEquals(factorise(2), expected = Map(BigInt(2) -> 1))
    assertEquals(factorise(17), expected = Map(BigInt(17) -> 1))
    assertEquals(factorise(75), expected = Map(BigInt(3) -> 1, BigInt(5) -> 2))
    assertEquals(factorise(30), expected = Map(BigInt(2) -> 1, BigInt(3) -> 1, BigInt(5) -> 1))
    assertEquals(factorise(8), expected = Map(BigInt(2) -> 3))
