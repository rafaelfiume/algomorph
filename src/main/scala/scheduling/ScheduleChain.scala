package scheduling

import ChainContinuity.*
import Rules.*

/**
 * Phantom types in action to encode chain continuity invariants at the type level.
 */
sealed trait ChainContinuity
object ChainContinuity:
  sealed trait Continuous extends ChainContinuity // no gaps, no conflicts
  sealed trait WithGaps extends ChainContinuity // gaps allowed, no conflicts
  // case NoGaps // no gaps, conflicts allowed ?
  // case Any gaps and conflicts allowed ?

sealed abstract class ScheduleChain[R, C <: ChainContinuity](val schedules: Seq[Schedule[R]])

object ScheduleChain:

  /**
   * Creates a `ScheduleChain` from a sequence of `Schedule`'s with full control over sorting, validation, and ChainContinuity
   * invariant.
   *
   * ===Usage Guidance===
   *   - Most users should favor the more specific smart constructors, such as `makeContinuous`, `makeWithGaps`
   *   - The caller is responsible for ensuring that the provided validation matches the chosen `ChainContinuity`'s type
   *   - Mismatches between validation logic and the parameter type might lead to runtime errors.
   *
   * ===Complexity===
   *
   * The time complexity depends on the `sort` function:
   *   - Best-case: Θ(n) when the list is already sorted and the validation is Θ(n)
   *   - Average-case: Θ(n log n) when using an effective sorting algorithm
   *   - Worst-case: Θ(n^2) or worse with poor sorting algorithm.
   *
   * Space complexity: Θ(n)
   *
   * Performance notes:
   *   - Use the `AssumeSorted` variants, or `identity` as the `sort` parameter for pre-sorted inputs
   *   - If unsure, use `_.sortBy(_.startMillis)` which provides Θ(n log n) time and Θ(n) space
   *   - For large datasets, consider batching or streaming at the application level.
   */
  def make[R, C <: ChainContinuity](schedules: Seq[Schedule[R]])(sort: Seq[Schedule[R]] => Seq[Schedule[R]])(
    validate: Rule[R]
  ): Either[List[RuleError[R]], ScheduleChain[R, C]] =
    val sorted = sort(schedules)
    val errors = validate(sorted)
    Either.cond(errors.isEmpty, new ScheduleChain[R, C](sorted) {}, errors)

  def makeContinuous[R](schedules: Seq[Schedule[R]]): Either[List[RuleError[R]], ScheduleChain[R, Continuous]] =
    make(schedules)(_.sortBy(_.startMillis))(continuous)

  def makeContinuousAssumeSorted[R](schedules: Seq[Schedule[R]]): Either[List[RuleError[R]], ScheduleChain[R, Continuous]] =
    make(schedules)(identity)(continuous)

  def makeWithGaps[R](schedules: Seq[Schedule[R]]): Either[List[RuleError[R]], ScheduleChain[R, WithGaps]] =
    make(schedules)(_.sortBy(_.startMillis))(noConflicts)

  def makeWithGapsAssumeSorted[R](schedules: Seq[Schedule[R]]): Either[List[RuleError[R]], ScheduleChain[R, WithGaps]] =
    make(schedules)(identity)(noConflicts)

  /**
   * For trusted contexts only, such as testing.
   */
  def unsafe[R, C <: ChainContinuity](schedules: Seq[Schedule[R]]): ScheduleChain[R, C] = new ScheduleChain[R, C](schedules) {}

  def merge[R](
    schedules: ScheduleChain[R, Continuous],
    overrides: ScheduleChain[R, WithGaps] | ScheduleChain[R, Continuous]
  ): ScheduleChain[R, Continuous] =
    (schedules.schedules, overrides.schedules) match
      case (s: Vector[Schedule[R]], o: Vector[Schedule[R]]) => merge(s, o)
      case (s, o)                                           => merge(s.toVector, o.toVector)

  private def merge[R](schedules: Vector[Schedule[R]], overrides: Vector[Schedule[R]]) = ???
