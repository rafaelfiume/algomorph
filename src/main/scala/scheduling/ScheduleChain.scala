package scheduling

import ChainContinuity.*
import Rules.*
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
 * Phantom types encoding chain continuity invariants at the type level.
 */
sealed trait ChainContinuity
object ChainContinuity:
  sealed trait Continuous extends ChainContinuity // no gaps, no conflicts
  sealed trait Gapped extends ChainContinuity // gaps allowed, no conflicts
  // case NoGaps // no gaps, conflicts allowed ?
  // case Any gaps and conflicts allowed ?

/**
 * Semantic chaining: each schedule in the chain is linked by a temporal invariant.
 *
 * For example: `Continuous` chains require all schedules to be adjacent `Gapped` chains allow gaps between schedules, but still
 * enforces ordering and non-overlapping.
 */
sealed abstract class ScheduleChain[R, C <: ChainContinuity](val schedules: Seq[Schedule[R]]):
  override def toString(): String = schedules.mkString("\n")

object ScheduleChain:

  /**
   * Creates a `ScheduleChain` from a sequence of `Schedule`'s with full control over sorting, validation, and ChainContinuity
   * invariant.
   *
   * ===Usage Guidance===
   *   - Most users should favor the more specific smart constructors, such as `makeContinuous`, `makeAllowingGaps`
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
  def make[R, C <: ChainContinuity](schedules: Schedule[R]*)(sort: Seq[Schedule[R]] => Seq[Schedule[R]])(
    validate: Rule[R]
  ): Either[List[RuleError[R]], ScheduleChain[R, C]] =
    val sorted = sort(schedules)
    val errors = firstOf(Rules.nonEmpty, validate)(sorted)
    Either.cond(errors.isEmpty, new ScheduleChain[R, C](sorted) {}, errors)

  def makeContinuous[R](schedules: Schedule[R]*): Either[List[RuleError[R]], ScheduleChain[R, Continuous]] =
    make(schedules*)(_.sortBy(_.startMillis))(continuous)

  def makeContinuousAssumeSorted[R](schedules: Schedule[R]*): Either[List[RuleError[R]], ScheduleChain[R, Continuous]] =
    make(schedules*)(identity)(continuous)

  def makeAllowingGaps[R](schedules: Schedule[R]*): Either[List[RuleError[R]], ScheduleChain[R, Gapped]] =
    make(schedules*)(_.sortBy(_.startMillis))(noConflicts)

  def makeAllowingGapsAssumeSorted[R](schedules: Schedule[R]*): Either[List[RuleError[R]], ScheduleChain[R, Gapped]] =
    make(schedules*)(identity)(noConflicts)

  /**
   * For trusted contexts only, such as testing.
   */
  def unsafe[R, C <: ChainContinuity](schedules: Seq[Schedule[R]]): ScheduleChain[R, C] = new ScheduleChain[R, C](schedules) {}

  def mergeStrict[R](
    base: ScheduleChain[R, Continuous],
    overrides: ScheduleChain[R, Gapped] | ScheduleChain[R, Continuous]
  ): Either[List[RuleError[R]], ScheduleChain[R, Continuous]] =
    (base.schedules, overrides.schedules) match
      case (b: List[Schedule[R]], o: List[Schedule[R]]) => doMerge(b, o)
      case (b, o)                                       => doMerge(b.toList, o.toList)

  private def doMerge[R](base: List[Schedule[R]],
                         overrides: List[Schedule[R]]
  ): Either[List[RuleError[R]], ScheduleChain[R, Continuous]] =
    @tailrec
    def loop(remainingBase: List[Schedule[R]],
             remainingOverride: List[Schedule[R]],
             merged: ListBuffer[Schedule[R]]
    ): ListBuffer[Schedule[R]] =
      (remainingBase, remainingOverride) match
        // both exhausted
        case (Nil, Nil) => merged

        // only overrides left
        case (Nil, os) => loop(Nil, Nil, merged.appendAll(os))

        // only base left
        case (bs, Nil) => loop(Nil, Nil, merged.appendAll(bs))

        // override *precedes* all base schedules
        case (b :: bs, o :: os) if o.endMillis <= b.startMillis =>
          loop(b :: bs, os, merged.append(o))

        // override *follows* next base
        case (b :: bs, o :: os) if o.startMillis >= b.endMillis =>
          loop(bs, o :: os, merged.append(b))

        // conflict
        case (b :: bs, o :: os) =>
          classifyOverlap(b, o) match
            case Overlap.Exact => loop(bs, os, merged.append(o))

            case Overlap.CoversStart =>
              val newB = Schedules.make(b.resource, o.endMillis, b.endMillis)
              loop(newB :: bs, os, merged.append(o))

            case Overlap.CoversEnd =>
              val newB = Schedules.make(b.resource, b.startMillis, o.startMillis)
              loop(bs, o :: os, merged.append(newB))

            case Overlap.Contained =>
              val pre = Schedules.make(b.resource, b.startMillis, o.startMillis)
              val pos = Schedules.make(b.resource, o.endMillis, b.endMillis)
              loop(pos :: bs, o :: os, merged.append(pre))

            case Overlap.FullCover => loop(bs, o :: os, merged)

    val merged = loop(base, overrides, ListBuffer.empty)
    ScheduleChain.makeContinuousAssumeSorted[R](merged.toSeq*)

  // Consider to expand classify to include the remaining temporal relations: precedes and follows.
  private def classifyOverlap[R](b: Schedule[R], o: Schedule[R]): Overlap =
    if o.startMillis == b.startMillis && o.endMillis == b.endMillis then Overlap.Exact
    else if o.startMillis <= b.startMillis && o.endMillis < b.endMillis then Overlap.CoversStart
    else if o.startMillis > b.startMillis && o.endMillis >= b.endMillis then Overlap.CoversEnd
    else if o.startMillis > b.startMillis && o.endMillis < b.endMillis then Overlap.Contained
    else if o.startMillis <= b.startMillis && o.endMillis >= b.endMillis then Overlap.FullCover
    else throw new AssertionError("unknown branch")

  enum Overlap:
    // override overlaps base exactly
    case Exact
    // override cover start of base
    case CoversStart
    // override cover end of base
    case CoversEnd
    // override fully overlapped by base
    case Contained
    // override fully overlaps base
    case FullCover
