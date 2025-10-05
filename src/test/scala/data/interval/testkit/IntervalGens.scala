package data.interval.testkit

import data.interval.*
import data.interval.Interval.*
import data.interval.Interval.AdjacencyType.*
import data.interval.BoundedAlgebra
import org.scalacheck.Gen
import scala.math.Integral.Implicits.given

object IntervalGens:

  type Factory[T, I <: Interval[T]] = (T, T) => I

  // ------------- Bounds (Raw Points) ----------- //

  def bounds[T: Integral: BoundedAlgebra: Gen.Choose]: Gen[(T, T)] =
    Gen.frequency(
      6 -> strictBounds,
      2 -> degenerateBounds,
      2 -> extremeBounds
    )

  def reversedBounds[T: Integral: BoundedAlgebra: Gen.Choose]: Gen[(T, T)] =
    strictBounds.map { case (start, end) => (end, start) } // start > end

  def strictBounds[T: Integral: BoundedAlgebra: Gen.Choose]: Gen[(T, T)] =
    strictBounds(offsetLeft = zero, offsetRight = zero)

  def strictBounds[T: Integral: BoundedAlgebra: Gen.Choose](offsetLeft: T, offsetRight: T): Gen[(T, T)] =
    for
      maxStart <- Gen.choose(infimum + offsetLeft, supremum - offsetRight) // avoids empty intervals
      minEnd <- Gen.choose(maxStart + one, supremum) // ensures minEnd > maxStart
      start <- Gen.choose(infimum, maxStart) // ensures start <= maxStart
      end <- Gen.choose(minEnd, supremum) // ensures start < maxStart < minEnd <= end
    yield (start, end)

  def degenerateBounds[T: BoundedAlgebra: Gen.Choose] =
    Gen.choose(infimum, supremum).map(x => (x, x))

  def extremeBounds[T: Integral: BoundedAlgebra] =
    Gen.oneOf(
      infimum -> infimum,
      supremum -> supremum,
      infimum -> supremum,
      infimum -> (infimum + one),
      (supremum - one) -> supremum
    )

  def enclosingPoints[T: Integral: Gen.Choose, I <: Interval[T]](interval: I): Gen[T] =
    require(!interval.isDegenerate)
    val (start, end) = interval match
      case _: Closed[T]                => (interval.start, interval.end)
      case _: Open[T]                  => (interval.start + one, interval.end - one)
      case _: HalfOpenRight[T]         => (interval.start + one, interval.end - one)
      case _: NonEmptyHalfOpenRight[T] => (interval.start, interval.end - one)
      case _: HalfOpenLeft[T]          => (interval.start + one, interval.end)
    Gen.choose(start, end)

  // ------------------ Intervals ------------------- //

  def intervals[T: Integral: BoundedAlgebra: Gen.Choose, I <: Interval[T]](using factory: Factory[T, I]): Gen[I] =
    bounds[T].map(factory.apply)

  def strictIntervals[T: Integral: Gen.Choose: BoundedAlgebra, I <: Interval[T]](using Factory[T, I]): Gen[I] =
    strictIntervals(offsetLeft = zero, offsetRight = zero)

  def strictIntervals[T: Integral: Gen.Choose: BoundedAlgebra, I <: Interval[T]](offsetLeft: T, offsetRight: T)(using
    factory: Factory[T, I]
  ): Gen[I] =
    strictBounds(offsetLeft, offsetRight).map(factory.apply)

  // ------------------ Relationships ---------------- //

  def overlappingIntervals[T: Integral: BoundedAlgebra: Gen.Choose, I <: Interval[T]](using factory: Factory[T, I]): Gen[(I, I)] =
    Gen.oneOf(fullOverlaps, partialOverlaps)

  def fullOverlaps[T: Integral: BoundedAlgebra: Gen.Choose, I <: Interval[T]](using factory: Factory[T, I]): Gen[(I, I)] =
    // [         ]
    //     [ ]
    for
      (startA, endA) <- strictBounds
      startB <- Gen.choose(startA, endA - one)
      endB <- Gen.choose(startB + one, endA)
    yield factory(startA, endA) -> factory(startB, endB)

  def partialOverlaps[T: Integral: BoundedAlgebra: Gen.Choose, I <: Interval[T]](using factory: Factory[T, I]): Gen[(I, I)] =
    def partialOverlapWith(interval: I) =
      val (startA, endA) = (interval.start, interval.end)
      for
        startB <- Gen.choose(startA + one, endA - one) // half open right
        endB <- Gen.choose(endA, supremum)
      yield interval -> factory(startB, endB)

    // [     ]
    //    [     ]
    def partialOverlapRight = strictIntervals.flatMap(partialOverlapWith)

    //    [     ]
    // [     ]
    def partialOverlapLeft = strictIntervals.flatMap(partialOverlapWith).map { case (a, b) => (b, a) }

    Gen.oneOf(partialOverlapRight, partialOverlapLeft)

  // strict no-touch (no intersecting)
  // [  ]
  //        [   ]
  def disjointIntervals[T: Integral: BoundedAlgebra: Gen.Choose, I <: Interval[T]](using factory: Factory[T, I]): Gen[(I, I)] =
    def nonOverlapWith(interval: I) =
      val (startA, endA) = (interval.start, interval.end)
      for
        startB <- Gen.choose(infimum, interval.start - two) // avoid empty intervals and overlapping with closed intervals
        endB <- Gen.choose(startB + one, startA - one)
      yield interval -> factory(startB, endB)

    Gen.oneOf(
      strictIntervals(offsetLeft = zero, offsetRight = two).flatMap(nonOverlapWith),
      strictIntervals(offsetLeft = zero, offsetRight = two).flatMap(nonOverlapWith).map { case (start, end) => (end, start) }
    )

  /*
   * [4, 6) and [6, 8) are adjacent (half open-right)
   * [4, 6] and [7, 8] are adjacent (closed intervals)
   */
  def adjacentIntervals[T: Integral: BoundedAlgebra: Gen.Choose, I <: Interval[T]](using
    factory: Factory[T, I],
    ops: IntervalAlgebra[T, I]
  ): Gen[(I, I)] =
    def fixedStartBound(start: T) = Gen.choose(start + one, supremum).map { end => factory(start, end) }
    def fixedEndBound(end: T) = Gen.choose(infimum, end - one).map { start => factory(start, end) }
    for
      point <- Gen.choose(infimum + one, supremum - one)
      intervalA <- fixedEndBound(point)
      intervalB <- fixedStartBound(start = ops.adjacencyType match
        case Meeting     => point
        case Consecutive => point + one
        case NonAdjacent => throw new IllegalArgumentException(s"no support for adjacent intervals"))
    yield (intervalA, intervalB)

  def nonOverlappingIntervals[T: Integral: BoundedAlgebra: Gen.Choose, I <: Interval[T]](using
    factory: Factory[T, I],
    ops: IntervalAlgebra[T, I]
  ): Gen[(I, I)] =
    Gen.oneOf(adjacentIntervals, disjointIntervals)

  private def zero[T](using num: Integral[T]) = num.fromInt(0)
  private def one[T](using num: Integral[T]) = num.fromInt(1)
  private def two[T](using num: Integral[T]) = num.fromInt(2)

  private def infimum[T](using bound: BoundedAlgebra[T]) = bound.infimum
  private def supremum[T](using bound: BoundedAlgebra[T]) = bound.supremum
