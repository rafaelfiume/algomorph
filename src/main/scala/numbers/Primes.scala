package numbers

import Math.sqrt
import scala.collection.mutable

object Primes:

  /**
   * Checks if a number is prime using trial division.
   */
  def isPrime(n: Int): Boolean =
    if n < 2 then false
    else if n == 2 then true
    else if n % 2 == 0 then false
    else primes.takeWhile(_ <= sqrt(n)).forall(n % _ != 0)

  /**
   * Lazily evaluates prime numbers.
   *
   * This implementation is stack-safe but very slow. It should be useful for reference purposes or when dealing with big primes
   * is not a requirement.
   *
   * ===Complexity (to the best of my knowledge)===
   *   - Space: Θ(1)
   */
  def primes: LazyList[Int] = 2 #:: LazyList.from(3, 2).filter(isPrime)

  /**
   * A basic implementation of Sieve of Eratosthenes.
   *
   * ===Algorithm===
   *
   * Given an integer > 1 and a set P = {2, 3, 4, ..., n}:
   *   1. For each p ∈ P where p² <= n:
   *      1. Remove all multiples of p from P except for p itself.
   *   1. The remaining elements in P are prime numbers.
   *
   * ===Evaluation Semantics===
   * {{{
   * sieve(25):
   *   - Initial: [2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
   *   - p=2: [2,3,5,7,9,11,13,15,17,19,21,23,25]
   *   - p=3: [2,3,5,7,11,13,17,19,21,23,25]
   *   - p=4: sieve(4) = false -> Skip
   *   - p=5: [2,3,5,7,11,13,17,19,21,23]
   *   - p=6: p > sqrt(25) -> StopS
   * }}}
   *
   * ===Complexity===
   *   - Time: Θ(n log log n) - to the best of my knowledge
   *   - Space: Θ(n) - constant factor improved ~8x by replacing `Array[Boolean]` with `BitSet` (i.e. bits vs. bytes)
   */
  def sieve(n: Int): Vector[Int] =
    require(n > 1)
    val sieve = mutable.BitSet(2 to n*)

    for
      p <- 2 to sqrt(n).toInt
      if sieve(p)
      multiple <- p * p to n by p
    do sieve -= multiple

    (2 to n).filter(sieve).toVector
