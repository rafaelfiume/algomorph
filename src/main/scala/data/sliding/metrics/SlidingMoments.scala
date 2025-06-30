package data.sliding.metrics

import data.sliding.metrics.Sliding
import scala.reflect.ClassTag
import data.sliding.core.SlidingWindow

case class Moments(mean: Double, variance: Double, standardDeviation: Double)

/**
 * Calculates the mean, the population variance and sliding standard deviation of a population.
 *
 * Those are equivalent to the 1st moment and 2nd central moment calculated using expected values (E[x^n]). A future release might
 * also inclue the 3rd (Skewness) and 4th central moment (Kurtois) as well.
 *
 * It uses a configurable, fixed-sized window to track the `n` most recent values and perform calculation in Θ(1) time per update.
 * This makes it ideal for streaming or near-real-time applications, where recomputing statistics from scratch is inefficient.
 *
 * ===Math===
 * The population variance is defined by:
 * {{{
 *   σ² = Σ(x - μ)² / N
 *      = Σ(x²) / N - μ²
 * }}}
 *
 * Or in expectation form:
 * {{{
 *   σ² = E[x²] − (E[x])²
 * }}}
 *
 * The standard deviation is:
 * {{{
 *   σ = √(E[x²] − (E[x])²)
 * }}}
 *
 * Where:
 *   - x = each value in the population
 *   - μ = mean of the population
 *   - N = number of values in the population.
 *
 * ===Population Variance===
 * This implementation uses the population variance (divided by _N_) instead of sample variance (divided by _N-1_) because:
 *   - The slding window is treated as the full population
 *   - This assumes that sample variance (Bessel's correction) is unnecessary in streaming applications
 *   - I believe this will lead to a more consistent local variance rather than trying to identify a global one.
 *
 * ===Complexity===
 *   - Time: `add` is performed in Θ(1)
 *   - Space: Θ(capacity) space due to a `CircularBuffer` allocation
 */
class SlidingMoments[T: ClassTag](size: Int)(using num: Numeric[T]) extends SlidingWindow[T, Moments]:
  require(size > 0, "window size of sliding average must be positive")

  private val slidingMeanSqr = Sliding.mean[T](size)
  private val slidingMean = Sliding.mean[T](size)

  override def add(x: T): Moments =
    val meanOfSquare = slidingMeanSqr.add(num.times(x, x)) // E[x^2]
    val mean = slidingMean.add(x) // E[x]
    val variance = meanOfSquare - (mean * mean)
    val stdDev = Math.sqrt(variance)
    Moments(mean, variance, stdDev)
