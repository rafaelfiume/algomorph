package io.rafaelfiume.algomorph.data.interval

/**
 * A type class for discrete domains, introduced to define [[Interval.isAdjacent]].
 *
 * Provides **total**, type-safe discrete successor and predecessor operations.
 *
 * Unlike non-total arithmetic functions that might overflow (e.g `succ(supremum)`), these functions explicitly signal *the
 * absence of* a value by returning Option[T].
 *
 * It is not possible to implement instances of this type class for continuous types.
 *
 * ===Laws===
 *   - `succ(x).exists(pred(_) == Some(x))`
 *   - `pred(x).exists(succ(_) == Some(x))`
 *   - `pred(x) < Some(x) && succ(x) > Some(x)` for ordered T
 */
trait DiscreteAlgebra[T]:
  def pred(value: T): Option[T]
  def succ(value: T): Option[T]

object DiscreteAlgebra:
  object instances:
    given DiscreteAlgebra[Int]:
      override def pred(value: Int): Option[Int] = if value == Int.MinValue then None else Some(value - 1)
      override def succ(value: Int): Option[Int] = if value == Int.MaxValue then None else Some(value + 1)

    given DiscreteAlgebra[Long]:
      override def pred(value: Long): Option[Long] = if value == Long.MinValue then None else Some(value - 1)
      override def succ(value: Long): Option[Long] = if value == Long.MaxValue then None else Some(value + 1)

  object syntax:
    extension [T](value: T)(using alg: DiscreteAlgebra[T])
      def pred(): Option[T] = alg.pred(value)
      def succ(): Option[T] = alg.succ(value)
