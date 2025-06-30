package data.sliding.core

import scala.reflect.ClassTag
import data.mutable

/**
 * An algebraic interface for computing a running aggregate over a sliding window.
 *
 * The `aggregate` in this context means combining multiple values into a single summary (e.g. sum, mean, max).
 */
trait SlidingWindow[I, O]:
  def add(value: I): O

/**
 * A sliding window for invertible groups - i.e. types that support `identity`, `combine` and `inverse` operations.
 *
 * It uses group properties to maintain a running aggregate over the last `n` values in Θ(1). Use it when the aggregation can be
 * both updated (via `combine`) and undone (via `inverse`). Examples: sum, mean, and othe associative operations with inverses.
 *
 * ===Complexity===
 *   - `add` is performed in Θ(1) time per update
 *   - Space is Θ(capacity) space due to a `CircularBuffer` allocation.
 */
class GroupSlidingWindow[@specialized(Int, Long, Float, Double) I: ClassTag, O](
  size: Int,
  // group ops
  identity: I,
  combine: (I, I) => I,
  inverse: I => I,
  //
  output: (I, I) => O
)(using num: Numeric[I])
    extends SlidingWindow[I, O]:
  require(size > 0, "window size of sliding average must be positive")

  private val buffer = mutable.CircularBuffer.make[I](size)
  private var acc: I = identity

  override def add(value: I): O =
    buffer.add(value) match
      case None =>
        acc = combine(acc, value)
        output(acc, num.fromInt(buffer.filled))
      case Some(old) =>
        acc = combine(value, combine(acc, inverse(old)))
        output(acc, num.fromInt(size))

/**
 * A sliding window for semigroups - i.e. types that support `combine` only.
 *
 * ===Complexity===
 *   - `add` is performed in Θ(n) time per update
 *   - Space is Θ(capacity) space due to a `CircularBuffer` allocation.
 */
class SemigroupSlidingWindow[@specialized(Int, Long, Float, Double, Boolean) I: ClassTag](
  size: Int,
  // semigroup ops
  combine: (I, I) => I
  //
) extends SlidingWindow[I, I]:
  require(size > 0, "window size of sliding average must be positive")

  private val buffer = mutable.CircularBuffer.make[I](size)

  override def add(value: I): I =
    val _ = buffer.add(value)
    buffer.iterator.reduce(combine)
