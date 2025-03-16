package red.book.ch10

import red.book.ch08.Gen.choose
import red.book.ch08.Prop.forAll
import red.book.ch08.{Gen, Prop}
import red.book.ch10.Monoids.*

object MonoidLaws extends App:

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = forAll(gen) { a =>
    m.op(m.zero, a) == a && a == m.op(a, m.zero)

  } && forAll(for
    a <- gen
    b <- gen
    c <- gen
  yield (a, b, c)) { case (a, b, c) =>
    m.op(a, m.op(b, c)) == m.op(m.op(a, b), c)
  }

  private val ints = choose(-10, 20)
  Prop.run(monoidLaws(intAddition, ints))
  Prop.run(monoidLaws(BrokenMonoids.subtraction, ints)) // This one doesn't pass
  Prop.run(monoidLaws(intMultiplication, ints))

  Prop.run(monoidLaws(booleanAnd, Gen.boolean))
  Prop.run(monoidLaws(booleanOr, Gen.boolean))

//    Prop.run(monoidLaws(endoMonoid[Int], genFn2(ints)((a1: Int, a2: Int) => a1 + a2))) // fails cause will try to compare functions with '=='

  Prop.run(monoidLaws(Monoids.monOrd, trackGen))
  Prop.run(monoidLaws(Monoids.monOrdBook, trackTracker))

  def trackGen: Gen[Track] = for
    i1 <- ints
    i2 <- ints
    r <- choose(1, 4)
    kind = r % 4
  yield if kind == 0 then TNumber(i1) else if kind == 1 then True() else if kind == 2 then False() else Interval(i1, i2)

  def booleans = Gen.boolean

  def trackTracker: Gen[Option[(Int, Int, Boolean)]] = for
    i1 <- ints
    i2 <- ints
    r <- choose(1, 5)
    maybeNone = r % 5 == 1
  yield if maybeNone then None else Some((i1, i2, i1 <= i2))

  /////////////// WC ////////////////////

  Prop.run(monoidLaws(WC.wcMonoid, wcGen))

  def wcGen: Gen[WC] = for
    i <- ints
    s <- Gen.stringN(i)
  yield Stub(s)

object BrokenMonoids:

  val subtraction = new Monoid[Int]:
    override def op(a1: Int, a2: Int) = a1 - a2
    override def zero = 0
