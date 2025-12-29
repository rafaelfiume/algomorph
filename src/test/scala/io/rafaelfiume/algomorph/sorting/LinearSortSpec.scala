package io.rafaelfiume.algomorph.sorting

import io.rafaelfiume.algomorph.sorting.LinearSort.*
import munit.ScalaCheckSuite
import org.scalacheck.{Gen, ShrinkLowPriority}
import org.scalacheck.Prop.*

import scala.reflect.ClassTag
import scala.util.Random

import Ordering.Implicits.given

class LinearSortSpec extends ScalaCheckSuite with ShrinkLowPriority:

  checkLinearSortingProperties("Char", Char.MinValue, Char.MaxValue)
  checkLinearSortingProperties("Byte", Byte.MinValue, Byte.MaxValue)
  checkLinearSortingProperties("Short", Short.MinValue, Short.MaxValue)
  checkLinearSortingProperties("Integer", Int.MinValue, Int.MaxValue)
  checkLinearSortingProperties("Long", Long.MinValue, Long.MaxValue)
  checkLinearSortingProperties("BigInt", BigInt(Long.MinValue).*(2), BigInt(Long.MaxValue).*(2))

  def checkLinearSortingProperties[T: Gen.Choose: ClassTag: Integral](domain: String, min: T, max: T): Unit =
    property(s"RadixSort sorts $domain numbers"):
      forAll(numberSequence(min, max)) { elems =>
        val result = LinearSort.sort(elems)

        isSorted(result) && preservesElements(result, elems)
      }

  property("Radix sort can sort large sequences"):
    val elems = (1 to 1_000_000).map(_ => Random.between(Int.MinValue, Int.MaxValue))

    val result = LinearSort.sort(elems)

    isSorted(result) && preservesElements(result, elems)

  property("CountingSort sorts non-negative numbers"):
    forAll(numberSequence(min = -10_000, max = 10_000)) { elems =>
      val result = CountingSort.sort(elems)

      isSorted(result) && preservesElements(result, elems)
    }

  private def isSorted[T: Integral](result: Seq[T]): Boolean =
    result.sliding(2).forall {
      case Seq(a, b)      => a <= b
      case Seq(a)         => true
      case s if s.isEmpty => true
    }

  private def preservesElements[T: Ordering](result: Seq[T], original: Seq[T]): Boolean =
    result.sorted == original.sorted

  private def numberSequence[T: Gen.Choose](min: T, max: T): Gen[Seq[T]] =
    for
      size <- Gen.choose(0, 10)
      list <- Gen.listOfN(size, Gen.choose(min, max))
    yield list.toSeq

  def nonNegativeNumberSequence: Gen[Seq[Int]] =
    for
      size <- Gen.choose(0, 10)
      list <- Gen.listOfN(size, Gen.choose(0, 1_000))
    yield list.toSeq
