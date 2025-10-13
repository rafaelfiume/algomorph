package data.interval.testkit

import data.interval.*
import data.interval.Interval.*
import data.interval.Interval.AdjacencyType.*
import data.interval.BoundedAlgebra
import org.scalacheck.Gen
import scala.math.Integral.Implicits.given
import scala.math.Ordering.Implicits.given

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

  def intersectingIntervals[T: Integral: BoundedAlgebra: Gen.Choose, I <: Interval[T]](using
    factory: Factory[T, I]
  ): Gen[(I, I)] =
    for
      a <- strictIntervals
      b <- intersectingWith(a)
    yield (a, b)

  private def intersectingWith[T: Integral: BoundedAlgebra: Gen.Choose, I <: Interval[T]](interval: => I)(using
    factory: Factory[T, I]
  ): Gen[I] = Gen.oneOf(
    containedBy(interval),
    intersectsRightward(interval)
  )

  // interval:    [--------------]
  // returns:         [----]
  private def containedBy[T: Integral: Gen.Choose, I <: Interval[T]](interval: I)(using
    factory: Factory[T, I]
  ): Gen[I] =
    require(interval.start < interval.end, s"interval=${interval} must not be degenerated")
    val (startA, endA) = interval.start -> interval.end
    for
      startB <- Gen.choose(startA, endA - one)
      endB <- Gen.choose(startB + one, endA)
    yield factory(startB, endB)

  // interval:    [--------]
  // returns:         [--------]
  private def intersectsRightward[T: Integral: BoundedAlgebra: Gen.Choose, I <: Interval[T]](interval: I)(using
    factory: Factory[T, I]
  ): Gen[I] =
    require(interval.start < interval.end, s"interval=${interval} must not be degenerated")
    val (startA, endA) = (interval.start, interval.end)
    for
      startB <- Gen.choose(startA, endA - one)
      endB <- Gen.choose(startB + one, supremum)
    yield factory(startB, endB)

  // Provides a monotonic intersecting chain of intervals.
  // Ensures each consecutive pair intersects:
  // [------]
  //     [---------]
  //         [--------------]
  def intersectingIntervalChain[T: Integral: BoundedAlgebra: Gen.Choose, I <: Interval[T]](using
    factory: Factory[T, I]
  ): Gen[Seq[I]] =
    for
      size <- Gen.choose(2, 99)
      head <- strictIntervals
      seq <- Gen.tailRecM(1 -> List(head)) {
        case (index, acc @ (prev :: _)) if index < size =>
          intersectingWith(prev).map { intersecting => Left((index + 1, intersecting :: acc)) }

        case (_, acc) => Right(acc.reverse)
      }
    yield seq.toSeq

  // strict no intersecting
  // [------]
  //            [------]
  def disjointIntervals[T: Integral: BoundedAlgebra: Gen.Choose, I <: Interval[T]](using factory: Factory[T, I]): Gen[(I, I)] =
    def nonIntersectingWith(interval: I) =
      val (startA, endA) = (interval.start, interval.end)
      for
        // avoids both empty intervals and handles intersection between closed intervals
        startB <- Gen.choose(infimum, interval.start - two)
        endB <- Gen.choose(startB + one, startA - one)
      yield interval -> factory(startB, endB)

    strictIntervals(offsetLeft = zero, offsetRight = two).flatMap(nonIntersectingWith)

  def disjointIntervalChain[T: Integral: BoundedAlgebra: Gen.Choose, I <: Interval[T]](using
    factory: Factory[T, I]
  ): Gen[Seq[I]] =
    for
      size <- Gen.choose(5, 99)
      seq <- Gen
        .listOfN(size, Gen.choose(infimum + one, supremum - one))
        .map(_.sorted.distinct.sliding(2, 3))
        .map(_.collect { case List(start, end) => factory(start, end) })
        .map(_.toSeq)
    yield seq

  /*
   * [4, 6) and [6, 8) are adjacent (half open-right)
   * [4, 6] and [7, 8] are adjacent (closed intervals)
   */
  def adjacentIntervals[T: Integral: BoundedAlgebra: Gen.Choose, I <: Interval[T]](using
    factory: Factory[T, I],
    alg: IntervalAlgebra[T, I]
  ): Gen[(I, I)] =
    def fixedStartBound(start: T) = Gen.choose(start + one, supremum).map { end => factory(start, end) }
    def fixedEndBound(end: T) = Gen.choose(infimum, end - one).map { start => factory(start, end) }
    for
      adjacentAt <- Gen.choose(infimum + one, supremum - one)
      intervalA <- fixedEndBound(adjacentAt)
      intervalB <- fixedStartBound(nextStartPoint(adjacentAt))
    yield (intervalA, intervalB)

  /**
   * Generates a monotonic sequence of adjacent intervals bounded by infimum and supremum such as:
   * {{{
   * Seq(
   *   [a0, a1),
   *   [a1, a2),
   *   ...
   *   [a(n-1), an)
   * )
   * }}}
   */
  def adjacentIntervalChain[T: Integral: BoundedAlgebra: Gen.Choose, I <: Interval[T]](using
    factory: Factory[T, I],
    alg: IntervalAlgebra[T, I]
  ): Gen[Seq[I]] =
    for
      size <- Gen.choose(2, 99)
      seq <- Gen
        .listOfN(size, Gen.choose(infimum + one, supremum - one))
        .map(_.sorted.distinct.sliding(2))
        .map(_.collect { case List(start, end) => factory(nextStartPoint(start), end) })
        .map(_.toSeq)
    yield seq

  def nonIntersectingIntervals[T: Integral: BoundedAlgebra: Gen.Choose, I <: Interval[T]](using
    factory: Factory[T, I],
    alg: IntervalAlgebra[T, I]
  ): Gen[(I, I)] = Gen.oneOf(adjacentIntervals, disjointIntervals)

  private def nextStartPoint[T: Integral, I <: Interval[T]](adjacentAt: T)(using alg: IntervalAlgebra[T, I]): T =
    alg.adjacencyType match
      case Meeting     => adjacentAt
      case Consecutive => adjacentAt + one
      case NonAdjacent => throw new IllegalArgumentException(s"no support for adjacent intervals")

  private def zero[T](using num: Integral[T]) = num.fromInt(0)
  private def one[T](using num: Integral[T]) = num.fromInt(1)
  private def two[T](using num: Integral[T]) = num.fromInt(2)

  private def infimum[T](using bound: BoundedAlgebra[T]) = bound.infimum
  private def supremum[T](using bound: BoundedAlgebra[T]) = bound.supremum
