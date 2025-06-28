package data.sliding.metrics

import scala.reflect.ClassTag
import data.sliding.core.GroupSlidingWindow
import data.sliding.core.SemigroupSlidingWindow
import scala.math.Numeric.Implicits.given

object Sliding:

  /**
   * Creates a sliding window calculator for the arithmetic mean of a population.
   *
   * ===Math===
   * {{{
   *   μ = Σx / N
   * }}}
   *
   * Where:
   *   - Σx = sum of all values in the population
   *   - N = number of values in the population
   *
   * Or, simply:
   * {{{
   *   μ = (x₁ + x₂ + ⋯ + xₙ) / n
   * }}}
   */
  def mean[T: ClassTag](size: Int)(using num: Numeric[T]) = new GroupSlidingWindow[T, Double](
    size,
    identity = num.zero,
    combine = num.plus,
    inverse = a => -a,
    output = (a, b) => a.toDouble / b.toDouble
  )

  def max[T: ClassTag]()(using num: Numeric[T]) = new SemigroupSlidingWindow[T](size = 3, num.max)

  def min[T: ClassTag]()(using num: Numeric[T]) = new SemigroupSlidingWindow[T](size = 3, num.min)
