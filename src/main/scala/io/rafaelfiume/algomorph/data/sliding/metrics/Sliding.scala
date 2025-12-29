package io.rafaelfiume.algomorph.data.sliding.metrics

import scala.reflect.ClassTag
import io.rafaelfiume.algomorph.data.sliding.core.GroupSlidingWindow
import io.rafaelfiume.algomorph.data.sliding.core.SemigroupSlidingWindow
import scala.math.Numeric.Implicits.given
import io.rafaelfiume.algomorph.data.sliding.core.SlidingWindow

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
  def mean[I: ClassTag](size: Int)(using num: Numeric[I]): SlidingWindow[I, Double] = new GroupSlidingWindow[I, Double](
    size,
    identity = num.zero,
    combine = num.plus,
    inverse = a => -a,
    output = (a, b) => a.toDouble / b.toDouble
  )

  def moments[I: ClassTag: Numeric](size: Int): SlidingWindow[I, Moments] = new SlidingMoments[I](size)

  /**
   * Computes the minimum value over a fixed-size sliding window.
   *
   * ===Monotonicity===
   * This is not monotonically decreasing since the minimum value might be evicted from the window.
   */
  def min[I: ClassTag](size: Int)(using num: Numeric[I]): SlidingWindow[I, I] = new SemigroupSlidingWindow[I](size, num.min)

  /**
   * Computes the maximum value over a fixed-size sliding window.
   *
   * ===Monotonicity===
   * This is not monotonically increasing since the maximum value might be evicted from the window.
   */
  def max[I: ClassTag](size: Int)(using num: Numeric[I]): SlidingWindow[I, I] = new SemigroupSlidingWindow[I](size, num.max)
