package red.book.ch08

import java.util.concurrent.{ExecutorService, Executors}

import red.book.ch08.Dsl.*
import red.book.ch08.Gen.*
import red.book.ch08.MoreGens.*
import red.book.ch08.Prop.forAll

import scala.Ordering.*

object SomeExamples extends App:

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
    sorted.isEmpty || sorted.zip(sorted.tail).forall { case (a, b) => a <= b }
  }
  Prop.run(sameSizeSortedListProp && sameElementsProp && sortedElements)

  /**
   * ****************************************** Pars *******************************************
   */

  val S: Gen[ExecutorService] = weighted(
    choose(20, 25).map(Executors.newFixedThreadPool) -> 0.75,
    unit(Executors.newCachedThreadPool()) -> 0.25
  )

  /**
   * ****************************************** takeWhile *******************************************
   */

  val listOfInts = choose(-100, 100).listOfN(choose(0, 30))

  println("takeWhile only return elements that holds predicate")
  val p: Int => Boolean = i => i % 2 == 0
  val holdsPredicate = forAll(listOfInts) { list =>
    list.takeWhile(p).forall(p)
  }
  Prop.run(holdsPredicate)

  val holdsPredicate2 = forAll(listOfInts ** intToBooleanGen) { case list ** p =>
    println(list.takeWhile(p))
    list.takeWhile(p).forall(p)
  }
  val firstElementOfRemainderListDoesNotMatchPredicate = forAll(listOfInts ** intToBooleanGen) { case list ** p =>
    val taken = list.takeWhile(p).size

    where(taken > list.size) {
      !p(list.drop(taken).head)
    }
  }
  Prop.run(holdsPredicate2 && firstElementOfRemainderListDoesNotMatchPredicate)

  // TODO 8.15......

  System.exit(0)
