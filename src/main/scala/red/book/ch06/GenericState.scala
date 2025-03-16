package red.book.ch06

import red.book.ch06.State.*

import scala.collection.immutable.List.empty

object RngState:
  // NB - this was called SimpleRNG in the book text

  type Rand[A] = State[RNG, A]

  val int: Rand[Int] = State(_.nextInt)

  val nonNegativeInt: Rand[Int] = nextNonNegativeInt

  def unit[A](a: A): Rand[A] = State(rng => (a, rng))

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = State(rng =>
    val (a, rng2) = s.run(rng)
    (f(a), rng2)
  )

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = ra.map2(rb)((_, _))

  def nextNonNegativeInt: Rand[Int] = State(rng =>
    val (n, nextRng) = rng.nextInt
    (if n < 0 then -(n + 1) else n, nextRng)
  )

  def double: Rand[Double] =
    for nnInt <- nextNonNegativeInt
    yield nnInt.doubleValue() / Int.MaxValue

  def _double: Rand[Double] = map(nonNegativeInt)(_.doubleValue() / Int.MaxValue)

  def intDouble: Rand[(Int, Double)] = State(rng =>
    val (i, iRng) = rng.nextInt
    val (d, dRng) = RNG.double(iRng)
    ((i, d), dRng)
  )

  def _intDouble: Rand[(Int, Double)] = both(int, _double)

  def doubleInt: Rand[(Double, Int)] = State(rng =>
    val ((i, d), nextRng) = intDouble.run(rng)
    ((d, i), nextRng)
  )

  def _doubleInt: Rand[(Double, Int)] = both(_double, int)

  def double3: Rand[(Double, Double, Double)] = State(rng =>
    val (d1, rng1) = double.run(rng)
    val (d2, rng2) = double.run(rng1)
    val (d3, rng3) = double.run(rng2)
    ((d1, d2, d3), rng3)
  )

  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def nonNegativeLessThan(n: Int): Rand[Int] = for
    i <- nonNegativeInt
    mod = i % n
    v <- if i + (n - 1) - mod >= 0 then unit(mod) else nonNegativeLessThan(n)
  yield v

case class State[S, +A](run: S => (A, S)):

  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap { a =>
      sb.map { b => f(a, b) }
    }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s =>
    val (v, s2) = run(s)
    f(v).run(s2)
  )

object State:

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](ls: List[State[S, A]]): State[S, List[A]] = ls.foldRight(unit[S, List[A]](empty)) { (x, acc) =>
    x.map2(acc)(_ :: _)
  }

  def traverse[S, A, B](ls: List[A])(op: A => State[S, B]): State[S, List[B]] = sequence {
    ls.map(op)
  } // TODO Very lazy way of implementing traverse... Improve this if up to this task

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  def modify[S](f: S => S): State[S, Unit] = for
    s <- get
    _ <- set(f(s))
  yield ()

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(unlocked: Boolean, candies: Int, coins: Int):
  def locked = !unlocked

object Machine:

  type Coins = Int
  type Candies = Int
  type MachineState = State[Machine, (Coins, Candies)]

  def simulateMachine(inputs: List[Input]): MachineState =
    for
      _ <- traverse(inputs)(op)
      s <- get
    yield (s.coins, s.candies)

  val op: Input => State[Machine, Unit] = {
    case Coin => modify(unlock)
    case Turn => modify(turn)
  }

  def unlock(m: Machine): Machine = if m.locked && m.candies > 0 then m.copy(unlocked = true, coins = m.coins + 1) else m
  def turn(m: Machine): Machine = if m.unlocked then m.copy(unlocked = false, candies = m.candies - 1) else m
