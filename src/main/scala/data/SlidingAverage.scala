package data

import scala.reflect.ClassTag
import scala.math.Numeric.Implicits.given

class SlidingAverage[@specialized(Int, Long, Float, Double) T: ClassTag](size: Int)(using num: Numeric[T]):
  require(size > 0, "window size of sliding average must positive")

  private val buffer = mutable.CircularBuffer.make[T](size)
  private var sum: T = num.zero

  def add(value: T): Double =
    buffer.add(value) match
      case None =>
        sum = sum + value
        sum.toDouble / buffer.filled
      case Some(old) =>
        sum -= old
        sum += value
        sum.toDouble / size
