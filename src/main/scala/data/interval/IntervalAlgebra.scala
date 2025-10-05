package data.interval

import data.interval.DiscreteAlgebra.syntax.*
import data.interval.Interval.*
import Ordering.Implicits.*

// Type class for ad-hoc polymorphism
// Same-shape law of type classes will ensure (or constrain, depending on your domain) to comparison between the same type of intervals.
trait IntervalAlgebra[T, I <: Interval[T]]:
  def allowsDegenerate: Boolean
  def isAdjacent(a: I, b: I): Boolean
  def contains(a: I, point: T): Boolean
  def contains(a: I, b: I): Boolean
  def overlaps(a: I, b: I): Boolean
  def adjacencyType: AdjacencyType

object IntervalAlgebra:
  object instances:
    given [T: Ordering](using d: DiscreteAlgebra[T]): IntervalAlgebra[T, Closed[T]] with

      override def allowsDegenerate: Boolean = false

      override def isAdjacent(a: Closed[T], b: Closed[T]): Boolean = a.end.succ().contains(b.start)

      /*
       * Closed intervals [a.start, a.end] and [b.start, b.end] do not overlap when:
       *   * a.end < b.start    ->    A entirely before B
       *   * b.end < a.start    ->    B entirely before A
       * 
       * Negate that condition to determine overlaps:
       * !(a.end < b.start || b.end < a.start) => a.end >= b.start && b.end >= a.start
       * 
       * Which leads to a.start <= b.end && b.start <= a.end
       * 
       * A:  [     ]        [     ]          [     ]
       * B:     [     ]       [ ]         [     ]
       */
      override def overlaps(a: Closed[T], b: Closed[T]): Boolean =
        a.start <= b.end && b.start <= a.end

      override def contains(a: Closed[T], point: T): Boolean =
        a.start <= point && point <= a.end

      override def contains(a: Closed[T], b: Closed[T]): Boolean =
        a.start <= b.start && b.end <= a.end

      override def adjacencyType: AdjacencyType = AdjacencyType.Consecutive

    given [T: Ordering] => IntervalAlgebra[T, Open[T]]:
      override def allowsDegenerate: Boolean = true

      override def isAdjacent(a: Open[T], b: Open[T]): Boolean = a.end == b.start

      /*
       * Open intervals (a.start, a.end) and (b.start, b.end) do not overlap when:
       *   * a.end <= b.start    ->    A entirely before B
       *   * b.end <= a.start    ->    B entirely before A
       * 
       * Negate that condition to determine overlaps:
       * !(a.end <= b.start || b.end <= a.start) => a.end > b.start && b.end > a.start
       * 
       * Which leads to: a.start < b.end && b.start > a.end
       */
      override def overlaps(a: Open[T], b: Open[T]): Boolean =
        a.start < b.end && b.start < a.end

      override def contains(a: Open[T], point: T): Boolean =
        a.start < point && point < a.end

      override def contains(a: Open[T], b: Open[T]): Boolean =
        a.start <= b.start && b.end <= a.end

      override def adjacencyType: AdjacencyType = AdjacencyType.NonAdjacent

    given [T: Ordering] => IntervalAlgebra[T, HalfOpenRight[T]]:
      override def allowsDegenerate: Boolean = true

      override def isAdjacent(a: HalfOpenRight[T], b: HalfOpenRight[T]): Boolean = a.end == b.start

      /*
       * Half- closed intervals [a.start, a.end) and [b.start, b.end) do not overlap when:
       *   * a.end <= b.start    ->    A entirely before B
       *   * b.end <= a.start    ->    B entirely before A
       * 
       * Negate that condition to determine overlaps:
       * !(a.end <= b.start || b.end <= a.start) => a.end > b.start && b.end > a.start
       * 
       * Which leads to: a.start < b.end && b.start > a.end
       */
      override def overlaps(a: HalfOpenRight[T], b: HalfOpenRight[T]): Boolean =
        a.start < b.end && b.start < a.end

      override def contains(a: HalfOpenRight[T], point: T): Boolean =
        a.start <= point && point < a.end

      override def contains(a: HalfOpenRight[T], b: HalfOpenRight[T]): Boolean =
        a.start <= b.start && b.end <= a.end

      override def adjacencyType: AdjacencyType = AdjacencyType.Meeting

    given [T: Ordering] => IntervalAlgebra[T, NonEmptyHalfOpenRight[T]]:
      override def allowsDegenerate: Boolean = false

      override def isAdjacent(a: NonEmptyHalfOpenRight[T], b: NonEmptyHalfOpenRight[T]): Boolean = a.end == b.start

      override def overlaps(a: NonEmptyHalfOpenRight[T], b: NonEmptyHalfOpenRight[T]): Boolean =
        a.start < b.end && b.start < a.end

      override def contains(a: NonEmptyHalfOpenRight[T], point: T): Boolean =
        a.start <= point && point < a.end

      override def contains(a: NonEmptyHalfOpenRight[T], b: NonEmptyHalfOpenRight[T]): Boolean =
        a.start <= b.start && b.end <= a.end

      override def adjacencyType: AdjacencyType = AdjacencyType.Meeting

    given [T: Ordering] => IntervalAlgebra[T, HalfOpenLeft[T]]:
      override def allowsDegenerate: Boolean = true

      override def isAdjacent(a: HalfOpenLeft[T], b: HalfOpenLeft[T]): Boolean = a.end == b.start

      /*
       * Half-closed intervals (a.start, a.end] and (b.start, b.end] do not overlap when:
       *   * a.end <= b.start    ->    A entirely before B
       *   * b.end <= a.start    ->    B entirely before A
       * 
       * Negate that condition to determine overlaps:
       * !(a.end <= b.start || b.end <= a.start) => a.end > b.start && b.end > a.start
       * 
       * Which leads to: a.start < b.end && b.start > a.end
       */
      override def overlaps(a: HalfOpenLeft[T], b: HalfOpenLeft[T]): Boolean =
        a.end > b.start && b.end > a.start

      override def contains(a: HalfOpenLeft[T], point: T): Boolean =
        a.start < point && point <= a.end

      override def contains(a: HalfOpenLeft[T], b: HalfOpenLeft[T]): Boolean =
        a.start <= b.start && b.end <= a.end

      override def adjacencyType: AdjacencyType = AdjacencyType.Meeting

  object syntax:
    extension [T, I <: Interval[T]](interval: I)(using ops: IntervalAlgebra[T, I])
      def isAdjacent(other: I): Boolean = ops.isAdjacent(interval, other)
      def overlaps(other: I): Boolean = ops.overlaps(interval, other)
      def contains(point: T): Boolean = ops.contains(interval, point)
      def contains(other: I): Boolean = ops.contains(interval, other)
