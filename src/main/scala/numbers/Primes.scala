package numbers

import Math.sqrt
import scala.collection.mutable
import scala.annotation.tailrec

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

  /**
   * Determine if `n` and `k` are relatively prime numbers.
   */
  def isCoprime(n: BigInt, k: BigInt): Boolean = gcd(n, k) == 1

  /**
   * Returns the greatest common divisor (GCD) between two positive intergers `n` and `k`.
   *
   * ===Algorithm===
   * Let the prime factorisations of `n` and `k` be:
   *   - n = p1^a1 * p2^a2 * ... * pn^an
   *   - k = p1^b1 * p2^b2 * ... * pn^bn
   * Then gcd(n, k) = p1^min(a1,b1) * p2^min(a2,b2) * ... * pn^min(an,bn).
   *
   * ===Complexity===
   *   - Time: Θ(√max(n,k)) - based on trial division up to √n or √k
   *   - Space: Θ(p) - where p is the number of distinct prime factors in `n`` or `k``
   */
  def gcd(n: BigInt, k: BigInt): BigInt =
    val fn = factorise(n)
    val fk = factorise(k)
    fn.keySet
      .intersect(fk.keySet)
      .map { p => p.pow(math.min(fn(p), fk(p))) }
      .product

  /**
   * Returns the least common multiple (LCM) between two positive intergers `n` and `k`.
   *
   * ===Algorithm===
   * Let the prime factorisations of `n` and `k` be:
   *   - n = p1^a1 * p2^a2 * ... * pn^an
   *   - k = p1^b1 * p2^b2 * ... * pn^bn
   * Then lcm(n, k) = p1^max(a1,b1) * p2^max(a2,b2) * ... * pn^max(an,bn).
   *
   * ===Complexity===
   *   - Time: Θ(√max(n,k)) - based on trial division up to √n or √k
   *   - Space: Θ(p) - where p is the number of distinct prime factors in `n`` or `k``
   */
  def lcm(n: BigInt, k: BigInt): BigInt =
    val fn = factorise(n)
    val fk = factorise(k)
    fn.keySet
      .union(fk.keySet)
      .map { p =>
        p.pow(math.max(fn.getOrElse(p, 0), fk.getOrElse(p, 0)))
      }
      .product

  /**
   * ===Evaluation Semantics===
   * {{{
   * factorise(8):
   * 1. Initial n == 8:
   *   loop(current=8, factor=2, acc={})
   *
   * 2. (8 % 2) == 0:
   *   loop(current=4, factor=2, acc={2->1})
   *
   * 3. (4 % 2) == 0:
   *   loop(current=2, factor=2, acc={2->2})
   *
   * 4. (2 == current):
   *   Return -> acc={2->3}
   * }}}
   *
   * Another example:
   * {{{
   * factorise(7):
   * 1. Initial n == 7:
   *   loop(current=7, factor=2, acc={})
   *
   * 2. (7 % 2) != 0:
   *   loop(current=7, factor=3, acc={})
   *
   * 3. (3 * 3) > 7:
   *   Return -> Map(7 -> 1)
   * }}}
   *
   * ===Complexity===
   *   - Time: Θ(√n)
   *   - Space: Θ(p) - where p = number of distinct prime factors of n
   */
  def factorise(n: BigInt): Map[BigInt, Int] =
    require(n > 0, "provide a positive numbers")

    @tailrec
    def loop(current: BigInt, factor: Int, acc: Map[BigInt, Int]): Map[BigInt, Int] =
      if factor == current then acc.updatedWith(factor)(_.map(_ + 1).orElse(Some(1)))
      else if factor * factor > n then acc.updated(current, 1)
      else if current % factor == 0 then
        loop(
          current / factor,
          factor,
          acc.updatedWith(factor)(_.map(_ + 1).orElse(Some(1)))
        )
      else loop(current, factor + 1, acc)

    if n == 1 then Map.empty else loop(n, 2, Map.empty)
