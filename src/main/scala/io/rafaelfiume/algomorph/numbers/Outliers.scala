package io.rafaelfiume.algomorph.numbers

import scala.annotation.tailrec

object Outliers:
  import io.rafaelfiume.algomorph.numbers.Outliers.Result.*

  /**
   * Finds the the outlier by recursively comparing groups (like using a balance scale) with the least amount of weighings - the
   * "heaviest ball" problem.
   *
   * The key idea is in Information Theory:
   *   - There are three possible outcomes when weighting the balls: left heavier, right heavier or balanced.
   *   - Let k = number of weighings, in order to distinguish N possibilities, we need 3^k >= N.
   *
   * For 9 balls, 3^2 = 9, so 2 weighings suffice.
   *
   * Requirements:
   *   - Exactly one candidate is *heavier* than the others (the outlier). If the input contains multiple unequal weights, the
   *     result will be nondeterministic.
   *   - For general purpose maximum selection, @see `List#max` function.
   *
   * ===Algorithm===
   *   - Determine the number of possibilities (for 9 balls where one of them is he heaviest, N = 9)
   *   - Grouping Strategy: split the balls into groups of ⌊N/3⌋, ⌊N/3⌋ and N - 2⌊N/3⌋
   *     - For example, if N = 11, balls will be split into groups of 3, 3 and 5
   *     - That is to match the 3 possible scale outcomes (ternary decidion tree) and maximize information gain
   *   - First weigthing: Compare the 2 groups with identical number of balls
   *     - If they are balanced, the outlier is on the third group
   *     - If one of them is the heaviest and contains more than one ball, recurse on the heaviest group
   *   - Subsequent weighings: split the remaining candidates into a new group of thirds
   *   - Repeat until k satisfies 3^k >= N.
   *
   * ===Information Theory Background===
   *   - Each weighing provides log₂(3) ≈ 1.58 bits of information
   *   - Minimum weighing to find the outlier will be log₃(N)
   *   - The ternary-grouping strategy matches the information-theoretic lower bound.
   *
   * ===Complexity===
   *   - Time: Θ(n) - where n = size of the sample
   *   - Space: Θ(1)
   *
   * Performance Notes:
   *   - T(n) = T(n/3) + 2n/3 => T(n) = T(n/3) + cn => T(n) = Θ(n)
   *     - Insight: to find the heavist ball, every ball must be "touched". Therefore, Ω)(n).
   *   - Uses `Vector` instead of `List` for fast random access and avoiding time complexity of Θ(n) due to `splitAt`,
   * improving constant factor.
   */
  def findByWeighing(sample: Vector[Int]): Int =
    require(sample.nonEmpty, "empty sample")

    @tailrec
    def loop(candidates: Vector[(Int, Int)]): Int = candidates match
      case h +: Nil => // N = 1
        h._2
      case fst +: snd +: Nil => // N = 2
        if fst._1 > snd._1 then fst._2 else snd._2
      case _ => // N > 3
        val size = candidates.size / 3
        val (first, remaining) = candidates.splitAt(size)
        val (second, third) = remaining.splitAt(size)
        val next = compare(first.map(_._1), second.map(_._1)) match
          case LeftHeavier  => first
          case RightHeavier => second
          case Balanced     => third
        loop(next)

    loop(sample.zipWithIndex)

  /**
   * Finds the outlier by taking a unique number of candidates from the sample and measuring the total weight once - the "heaviest
   * pill" problem. The outlier can be either heavier or lighter than the normal pills.
   *
   * Requirements:
   *   - Exactly one candidate in the sample (the outlier) has a different known weight (e.g. 1.1g instead of the normal 1g).
   *
   * ===Algorithm===
   * Let samples be 20 bottles, where 19 contain pills weighing 1.0g, and 1 bottle has pills weighing 1.1g. Therefore, the known
   * weight delta between normal and outlier pills is 0.1. The goal is to find the outlier bottle k.
   *
   *   - Take a unique number of pills from each bottle (e.g., 1 pill from Bottle 1, 2 pills from Bottle 2, ..., 20 pills from
   *     Bottle 20), and weigh them together once.
   *   - Compute the expected weight if there were no outliers:
   *     - W_expected = Σ (from i=1 to 20) i × 1g = (20 × 21)/2 = 210g
   *   - Calculate the delta between actual and expected weights:
   *     - ΔW = W_actual − W_expected
   *   - Find k:
   *     - k = ΔW / weightDelta => k = ΔW / 0.1
   *
   * ===Complexity===
   *   - Time: Θ(n) - where n = size of the sample
   *   - Space: Θ(1)
   */
  def findByEncodedSum(sample: Vector[Double], weightDelta: Double): Int =
    require(sample.nonEmpty, "empty sample")
    require(weightDelta > 0, "known weight delta must be provided")

    val normalWeight = if sample.count(_ == sample.max) == 1 then sample.min else sample.max
    val expectedWeight = (1 to sample.size).sum * normalWeight
    val actualWeight = sample.zipWithIndex.map { (w, i) => (i + 1) * w }.sum
    val excessWeight = actualWeight - expectedWeight
    val signum = math.signum(excessWeight)
    val k = excessWeight / weightDelta * signum
    math.round(k).toInt - 1 // 0-based index

  private def compare(firstGroup: Vector[Int], secondGroup: Vector[Int]): Result =
    val fstTotalWeight = firstGroup.sum
    val sndTotalWeight = secondGroup.sum
    if fstTotalWeight > sndTotalWeight then LeftHeavier
    else if fstTotalWeight < sndTotalWeight then RightHeavier
    else Balanced

  private enum Result:
    case LeftHeavier
    case RightHeavier
    case Balanced
