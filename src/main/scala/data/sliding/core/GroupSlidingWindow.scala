package data.sliding.core

import scala.reflect.ClassTag
import data.mutable

/**
 * A sliding windown for invertible groups (i.e. supports combine and inverse operations).
 */
class GroupSlidingWindow[@specialized(Int, Long, Float, Double) T: ClassTag, R](
  size: Int,
  // group ops
  zero: T,
  combine: (T, T) => T,
  inverse: T => T,
  //
  output: (T, T) => R
)(using num: Numeric[T]):
  require(size > 0, "window size of sliding average must positive")

  private val buffer = mutable.CircularBuffer.make[T](size)
  private var sum: T = zero

  def add(value: T): R =
    buffer.add(value) match
      case None =>
        sum = combine(sum, value)
        output(sum, num.fromInt(buffer.filled))
      case Some(old) =>
        sum = combine(value, combine(sum, inverse(old)))
        output(sum, num.fromInt(size))
