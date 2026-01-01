package io.rafaelfiume.algomorph.data.interval

import io.rafaelfiume.algomorph.data.interval.DiscreteAlgebra.syntax.*
import io.rafaelfiume.algomorph.data.interval.Interval.*

import Ordering.Implicits.*

// Type class for ad-hoc polymorphism
// Same-shape law of type classes will constrain comparisons to same type of intervals.
trait IntervalAlgebra[T, I <: Interval[T]]:
  def validBounds(a: T, b: T): Boolean
  def contains(a: I, point: T): Boolean
  def contains(a: I, b: I): Boolean
  def intersects(a: I, b: I): Boolean
  // Extract the following to DiscreteIntervalAlgebra
  def adjacencyType: AdjacencyType
  def isAdjacent(a: I, b: I): Boolean = adjacentStart(a).forall(_ == b.start)
  def adjacentStart(a: I): Option[T]

object IntervalAlgebra:
  object instances:
    given [T: Ordering](using d: DiscreteAlgebra[T]): IntervalAlgebra[T, Closed[T]] with

      override def validBounds(a: T, b: T): Boolean = a <= b

      override def adjacencyType: AdjacencyType = AdjacencyType.Consecutive

      def adjacentStart(a: Closed[T]): Option[T] = a.end.succ()

      /*
       * Closed intervals [a.start, a.end] and [b.start, b.end] do not intersect when:
       *   * a.end < b.start    ->    A entirely before B
       *   * b.end < a.start    ->    B entirely before A
       * 
       * Negate that condition to determine intersection:
       * !(a.end < b.start || b.end < a.start) => a.end >= b.start && b.end >= a.start
       * 
       * Which leads to a.start <= b.end && b.start <= a.end
       * 
       * A:  [     ]        [     ]          [     ]
       * B:     [     ]       [ ]         [     ]
       */
      override def intersects(a: Closed[T], b: Closed[T]): Boolean =
        a.start <= b.end && b.start <= a.end

      override def contains(a: Closed[T], point: T): Boolean =
        a.start <= point && point <= a.end

      override def contains(a: Closed[T], b: Closed[T]): Boolean =
        a.start <= b.start && b.end <= a.end

    given [T: Ordering] => IntervalAlgebra[T, Open[T]]:
      override def validBounds(a: T, b: T): Boolean = a < b

      override def adjacencyType: AdjacencyType = AdjacencyType.NonAdjacent

      override def isAdjacent(a: Open[T], b: Open[T]): Boolean = false

      def adjacentStart(a: Open[T]): Option[T] = None

      override def intersects(a: Open[T], b: Open[T]): Boolean =
        a.start < b.end && b.start < a.end

      override def contains(a: Open[T], point: T): Boolean =
        a.start < point && point < a.end

      override def contains(a: Open[T], b: Open[T]): Boolean =
        a.start <= b.start && b.end <= a.end

    given [T: Ordering] => IntervalAlgebra[T, HalfOpenRight[T]]:
      override def validBounds(a: T, b: T): Boolean = a < b

      override def adjacencyType: AdjacencyType = AdjacencyType.Meeting

      def adjacentStart(a: HalfOpenRight[T]): Option[T] = Some(a.end)

      override def intersects(a: HalfOpenRight[T], b: HalfOpenRight[T]): Boolean =
        a.start < b.end && b.start < a.end

      override def contains(a: HalfOpenRight[T], point: T): Boolean =
        a.start <= point && point < a.end

      override def contains(a: HalfOpenRight[T], b: HalfOpenRight[T]): Boolean =
        a.start <= b.start && b.end <= a.end

    given [T: Ordering] => IntervalAlgebra[T, HalfOpenLeft[T]]:
      override def validBounds(a: T, b: T): Boolean = a < b

      override def adjacencyType: AdjacencyType = AdjacencyType.Meeting

      def adjacentStart(a: HalfOpenLeft[T]): Option[T] = Some(a.end)

      override def intersects(a: HalfOpenLeft[T], b: HalfOpenLeft[T]): Boolean =
        a.start < b.end && b.start < a.end

      override def contains(a: HalfOpenLeft[T], point: T): Boolean =
        a.start < point && point <= a.end

      override def contains(a: HalfOpenLeft[T], b: HalfOpenLeft[T]): Boolean =
        a.start <= b.start && b.end <= a.end

  object syntax:
    extension [T, I <: Interval[T]](interval: I)(using alg: IntervalAlgebra[T, I])
      def isAdjacent(other: I): Boolean = alg.isAdjacent(interval, other)
      def intersects(other: I): Boolean = alg.intersects(interval, other)
      def contains(point: T): Boolean = alg.contains(interval, point)
      def contains(other: I): Boolean = alg.contains(interval, other)
