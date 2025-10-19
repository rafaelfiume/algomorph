package sorting

import munit.ScalaCheckSuite
import org.scalacheck.Prop.*
import org.scalacheck.Gen
import org.scalacheck.ShrinkLowPriority

class CountingSortSpec extends ScalaCheckSuite with ShrinkLowPriority:

  property("sorts non-negative numbers"):
    forAll(nonNegativeNumberSequence) { numbers =>
      val result = CountingSort.sort(numbers)

      result.sliding(2).forall {
        case Seq(a, b)      => a <= b
        case Seq(a)         => true
        case s if s.isEmpty => true
      }
    }

  private def nonNegativeNumberSequence: Gen[List[Int]] =
    for
      size <- Gen.choose(0, 10)
      list <- Gen.listOfN(size, Gen.choose(-1_000, 1_000))
    yield list
