package numbers

import scala.annotation.tailrec

object Outliers:
  import numbers.Outliers.Result.*

  /**
   * Finds the index of the heaviest ball (the outlier) using a balance scale with the least amount of weighings.
   *
   * The key idea is in Information Theory:
   *   - There are three possible outcomes when weighting the balls: left heavier, right heavier or balanced.
   *   - Let k = number of weighings, in order to distinguish N possibilities, we need 3^k >= N.
   *
   * For 9 balls, 3^2 = 9, so 2 weighings suffice.
   *
   * Note:
   *   - This function assumes that exactly one ball is heavier than the others (the outlier). If the input contains multiple
   *     unequal weights, the result will be nondeterministic.
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
   *   - Time: Θ(n) - where n = size of balls
   *   - Space: Θ(1)
   *
   * Performance Notes:
   *   - T(n) = T(n/3) + 2n/3 => T(n) = T(n/3) + cn => T(n) = Θ(n)
   *     - Insight: to find the heavist ball, every ball must be "touched". Therefore, Ω)(n).
   *   - Uses `Vector` instead of `List` for fast random access and avoiding time complexity of Θ(n) due to `splitAt`,
   * improving constant factor.
   */
  def find(balls: Vector[Int]): Int =
    require(balls.nonEmpty, "balls can't be empty")

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

    loop(balls.zipWithIndex)

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
