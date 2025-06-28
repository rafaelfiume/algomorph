package data.sliding.core

import scala.reflect.ClassTag
import data.mutable

/**
 * A sliding windown for invertible groups (i.e. supports `identity`, `combine`` and `inverse` operations).
 */
class GroupSlidingWindow[@specialized(Int, Long, Float, Double) T: ClassTag, R](
  size: Int,
  // group ops
  identity: T,
  combine: (T, T) => T,
  inverse: T => T,
  //
  output: (T, T) => R
)(using num: Numeric[T]):
  require(size > 0, "window size of sliding average must positive")

  private val buffer = mutable.CircularBuffer.make[T](size)
  private var acc: T = identity

  def add(value: T): R =
    buffer.add(value) match
      case None =>
        acc = combine(acc, value)
        output(acc, num.fromInt(buffer.filled))
      case Some(old) =>
        acc = combine(value, combine(acc, inverse(old)))
        output(acc, num.fromInt(size))

/**
 * A sliding windown for semigroups (i.e. supports `combine` only operation).
 */
class SemigroupSlidingWindow[@specialized(Int, Long, Float, Double, Boolean) T: ClassTag](
  size: Int,
  // semigroup ops
  combine: (T, T) => T
  //
):
  require(size > 0, "window size of sliding average must positive")

  private val buffer = mutable.CircularBuffer.make[T](size)

  def add(value: T): T =
    val _ = buffer.add(value)
    buffer.iterator.reduce(combine)
