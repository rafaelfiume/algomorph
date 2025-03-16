package red.book.ch06

import red.book.ch06.RNG.*

import scala.annotation.tailrec
import scala.collection.immutable.List.empty

object RollingDie:

  def rollDie: Rand[Int] = map(RNG.nonNegativeLessThan(6))(_ + 1)

trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed =
        (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n =
        (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  val nonNegativeInt: Rand[Int] = nextNonNegativeInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a))) // 6.9 TODO Test this if feeling inspired

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def nextNonNegativeInt(rng: RNG): (Int, RNG) =
    val (n, nextRng) = rng.nextInt
    (if n < 0 then -(n + 1) else n, nextRng)

  def double(rng: RNG): (Double, RNG) =
    val (positive, nextRng) = nextNonNegativeInt(rng)
    (positive.doubleValue() / Int.MaxValue, nextRng)

  def _double: Rand[Double] = map(nonNegativeInt)(_.doubleValue() / Int.MaxValue)

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (i, iRng) = rng.nextInt
    val (d, dRng) = RNG.double(iRng)
    ((i, d), dRng)

  def _intDouble: Rand[(Int, Double)] = both(int, _double)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val ((i, d), nextRng) = intDouble(rng)
    ((d, i), nextRng)

  def _doubleInt: Rand[(Double, Int)] = both(_double, int)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    @tailrec
    def go(list: List[Int], count: Int)(rng: RNG): (List[Int], RNG) = count match
      case n if n < 0  => sys.error("count must be > 0")
      case n if n == 0 => (list, rng)
      case n if n > 0 =>
        val (i, nextRng) = rng.nextInt
        go(i :: list, count - 1)(nextRng)
    go(Nil, count)(rng)

  def _ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => // lifting a normal function to operate one a function that operates with Rand's
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = // TODO Test this if feeling inspired
    flatMap(ra) { a =>
      mapViaFlatMap(rb) { b => f(a, b) }
    }

  def sequence[A](ls: List[Rand[A]]): Rand[List[A]] = ls.foldRight(unit(empty[A])) { (x, acc) => map2(x, acc)(_ :: _) }

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = rng =>
    val (a, r2) = r(rng)
    f(a)(r2)

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if i + (n - 1) - mod >= 0 then unit(mod) else nonNegativeLessThan(n)
    }
