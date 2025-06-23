package data.sliding.metrics

import scala.reflect.ClassTag
import data.sliding.core.GroupSlidingWindow
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
    zero = num.zero,
    combine = num.plus,
    inverse = a => -a,
    output = (a, b) => a.toDouble / b.toDouble
  )

  // implement it possibly with a different abstraction that doesn't require zero nor inverse?
  def max[T: ClassTag](size: Int)(using num: Numeric[T]) = ???
