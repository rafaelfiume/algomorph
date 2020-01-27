package red.book.ch08

import java.util.concurrent.{ ExecutorService, Executors }

import red.book.ch07.Par
import red.book.ch07.Par.{ Par, lazyUnit }
import red.book.ch08.Gen._
import red.book.ch08.Prop.forAll
import red.book.ch08.Dsl._
import red.book.ch08.MoreGens._

import Ordering._

object SomeExamples extends App {

  println("There should be no items in the list greater than max")
  val smallInt = Gen.choose(-10, 10)
  val maxProp = forAll(listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }
  Prop.run(maxProp)

  println("Sorted list should have the same size as and elements of the unsorted one")
  val sameSizeSortedListProp = forAll(listOf(smallInt)) { ns =>
    ns.size == ns.sorted.size
  }
  val sameElementsProp = forAll(listOf(smallInt)) { ns =>
    val sorted = ns.sorted
    ns.forall(sorted.contains(_)) && sorted.forall(ns.contains(_))
  }
  val sortedElements = forAll(listOf(smallInt)) { ns =>
    val sorted = ns.sorted
    sorted.isEmpty || (sorted zip sorted.tail forall { case (a, b) => a <= b })
  }
  Prop.run(sameSizeSortedListProp && sameElementsProp && sortedElements)

  /******************************************** Pars ********************************************/

  val S: Gen[ExecutorService] = weighted(
    choose(20, 25).map(Executors.newFixedThreadPool) -> 0.75,
    unit(Executors.newCachedThreadPool()) -> 0.25
  )

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop = forAll(S ** g) { case ex ** v => f(v)(ex).get }
  def forAllPar[A](g: SGen[A])(f: A => Par[Boolean]): Prop = forAll(S.unsized ** g) { case ex ** v => f(v)(ex).get }

  private val es = Executors.newCachedThreadPool()

  def equal[A](p1: Par[A], p2: Par[A]): Par[Boolean] = Par.map2(p1, p2)(_ == _)

  implicit class ParEqual[A](p1: Par[A]) {
    def equal(p2: Par[A]) = SomeExamples.this.equal(p1, p2)
  }

  def checkPar(f: => Par[Boolean]): Prop = forAllPar(unit(()))(_ => f)

  println("Checking par unit properties...")

  val p1 = Prop.check {
    equal(
      p1 = Par.map(Par.unit(1))(_ + 1),
      p2 = Par.unit(2)
    )(es).get
  }
  Prop.run(p1)

  val p2 = checkPar {
    equal(
        p1 = Par.map(Par.unit(1))(_ + 1),
        p2 = Par.unit(2)
    )
  }
  Prop.run(p2)

  val pint = Gen.choose(0, 10) map (Par.unit(_))

  val pint2: SGen[Par[Int]] = Gen.listOf(Gen.choose(0, 1000)).map {
    list => Par.map { Par.sequence(list map (lazyUnit(_))) } { l => l.sum }
  }

  val pint3: Gen[Par[Int]] = choose(-500, 500).listOfN(choose(0, 20)) map { list =>
    list.foldLeft(lazyUnit(0))((par, int) => {
      Par.fork { Par.map2(par, lazyUnit(int))(_ + _) } // Current implementation of Par blocks with a fixed-size thread pool that is not big enough to handle all the forked computation
    })
  }
  val p4 = forAllPar(pint3)(p => {
    Par.map(p)(y => y) equal p
  })
  Prop.run(p4)

  println("fork(x) == x")
  val forkProperty = forAllPar(pint2) { p => // Using pint2 which doesn't fork process so we don't dead lock
    Par.fork(p) equal p
  }
  Prop.run(forkProperty)

  /******************************************** takeWhile ********************************************/

  val listOfInts = choose(-100,100).listOfN(choose(0, 30))

  println("takeWhile only return elements that holds predicate")
  val p: Int => Boolean = i => i % 2 == 0
  val holdsPredicate = forAll(listOfInts) { list =>
    list takeWhile p forall p
  }
  Prop.run(holdsPredicate)

  val holdsPredicate2 = forAll(listOfInts ** intToBooleanGen) { case list ** p =>
    println(list takeWhile p)
    list takeWhile p forall p
  }
  val firstElementOfRemainderListDoesNotMatchPredicate = forAll(listOfInts ** intToBooleanGen) { case list ** p =>
    val taken = list takeWhile p size

    where (taken > list.size) {
      !p(list.drop(taken).head)
    }
  }
  Prop.run(holdsPredicate2 && firstElementOfRemainderListDoesNotMatchPredicate)

  // TODO 8.15......

  System.exit(0)

}
