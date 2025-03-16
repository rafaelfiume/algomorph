package red.book.ch08

import red.book.ch04.{Option, *}
import red.book.ch05.Stream
import red.book.ch06.State.*
import red.book.ch06.{RNG, RngState, State}
import red.book.ch08.Prop.*

import scala.language.postfixOps
import scala.{Either as _, None as _, Option as _, Some as _, Stream as _}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
 */

case class Prop(run: (MaxSize, TestCases, RNG) => Result):

  def &&(o: Prop): Prop = Prop((max, n, rng) =>
    run(max, n, rng) match
      case Passed | Proved => o.run(max, n, rng)
      case x               => x
  )

  def ||(o: Prop): Prop = Prop((max, n, rng) =>
    run(max, n, rng) match
      case Falsified(msg, _) => o.tag(msg).run(max, n, rng)
      case x                 => x
  )

  def tag(msg: String): Prop = Prop((max, n, rng) =>
    run(max, n, rng) match
      case Falsified(f, c) => Falsified(msg + "\n" + f, c)
      case p               => p
  )

object Prop:

  type MaxSize = Int
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int

  sealed trait Result:
    def isFalsified = false
  case object Proved extends Result
  case object Passed extends Result
  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result:
    override def isFalsified: Boolean = true

  def apply(run: (TestCases, RNG) => Result): Prop = Prop { (_, n, rng) => run(n, rng) }

  def check[A](f: => Boolean): Prop = Prop { (_, _, _) => if f then Proved else Falsified("()", 0) }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop((max, n, rng) =>
    val casesPerSize = (n + (max - 1)) / max
    val props: Stream[Prop] = Stream.from(0).take((n.min(max)) + 1).map(i => forAll(g(i))(f))
    val prop: Prop = props.map(p => Prop { (max, _, rng) => p.run(max, casesPerSize, rng) }).toList.reduce(_ && _)
    prop.run(max, n, rng)
  )

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (n, rng) =>
    randomStream(as)(rng)
      .zip(Stream.from(0))
      .take(n)
      .map { case (a, i) =>
        try if f(a) then Passed else Falsified(a.toString, i)
        catch case e: Exception => Falsified(buildMsg(a, e), i)
      }
      .find(_.isFalsified)
      .getOrElse(Passed)
  }
  private def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
  private def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n${e.getStackTrace.mkString("\n")}"

  def run(p: Prop, maxSize: MaxSize = 100, testCases: TestCases = 100, rng: RNG = RNG.Simple(System.currentTimeMillis())): Unit =
    p.run(maxSize, testCases, rng) match
      case Falsified(msg, n) =>
        println(s"Falsified after $n passed tests:\n$msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")

object `**`:
  def unapply[A, B](p: (A, B)) = scala.Some(p)

case class Gen[A](sample: State[RNG, A]):

  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f.andThen(_.sample)))

  def map2[B, C](b: Gen[B])(f: (A, B) => C): Gen[C] = Gen(sample.map2(b.sample)(f))

  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap { listOfN }

  def listOfl: SGen[List[A]] = Gen.listOf1(this)

  def streamOfN(size: Int): Gen[Stream[A]] = Gen.streamOfN(size, this)

  def streamOfN(size: Gen[Int]): Gen[Stream[A]] = size.flatMap { streamOfN }

  def unsized: SGen[A] = SGen(forSize = _ => this) // this is defined as implicit in the authors' answers

  def **[B](g: Gen[B]): Gen[(A, B)] = this.map2(g)((_, _))

object Gen:

  def unit[A](a: => A): Gen[A] = Gen[A](RngState.unit(a))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(
    RngState.nonNegativeInt.map(n => start + n % (stopExclusive - start))
  )

  def boolean: Gen[Boolean] = Gen(choose(0, 2).sample.map(_ % 2 == 0))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(sequence(List.fill(n)(g.sample)))

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(forSize => g.listOfN(forSize))

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n.max(1)))

  def streamOfN[A](n: Int, g: Gen[A]): Gen[Stream[A]] = listOfN(n, g).map { list => Stream(list*) }

  def stringN(n: Int): Gen[String] = choose(32, 127).listOfN(n).map { list =>
    list.map { _.toChar } mkString
  }

  def string: SGen[String] = SGen(stringN)

  def genStringToInt(g: Gen[Int]): Gen[String => Int] = g.map { i => s => s.length + i }
  def genStringToInt2(g: Gen[Int]): Gen[String => Int] = genFn2(g)((i, s) => s.length + i)

  def genFn2[A, B](g: Gen[A])(f: (A, B) => A): Gen[B => A] = g.map { a => b => f(a, b) }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b => if b then g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    val g1Threshold = g1._2.abs / (g1._2 + g2._2.abs)
    Gen(RngState.double.flatMap { d =>
      if d < g1Threshold then g1._1.sample else g2._1.sample
    })

  /**
   * * The importance of playing **
   */
  def intTuple(start: Int, stop: Int): Gen[(Int, Int)] = choose(start, stop).map2(choose(start, stop))((_, _))

  def option[A](gen: Gen[A]): Gen[Option[A]] = choose(1, 21).flatMap { i =>
    if i == 10 then unit(None) else gen.map(Some(_))
  }

  def fromOption[A](gen: Gen[Option[A]]): Gen[A] = gen.flatMap {
    case Some(v) => unit(v)
    case None    => fromOption(gen)
  }

//case class SGen[+A](forSize: Int => Gen[A]) { It doesn't work with covariance
case class SGen[A](forSize: Int => Gen[A]):
  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A => B): SGen[B] = SGen { forSize(_).map(f) }

  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen { n =>
    forSize(n).flatMap { f(_)(n) }
  }

  def **[B](s2: SGen[B]): SGen[(A, B)] = for
    a <- this
    b <- s2
  yield (a, b)
