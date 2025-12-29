package io.rafaelfiume.algomorph.scheduling

import io.rafaelfiume.algomorph.scheduling.Schedule.TimeRange

import java.time.{DayOfWeek, ZoneId}
import java.time.DayOfWeek.*
import scala.annotation.tailrec

enum RuleError[R]:
  case NoSchedules[R]() extends RuleError[R]
  case Structural(a: Schedule[R], b: Schedule[R], message: String)
  case Individual(a: Schedule[R], message: String)

type Rule[R] = Seq[Schedule[R]] => List[RuleError[R]]

object Rules:

  /* ------- primitives ------- */

  def nonEmpty[R]: Rule[R] = schedules => if schedules.isEmpty then List(RuleError.NoSchedules[R]()) else Nil

  /**
   * Validates no gaps and no conflicts between schedules.
   *
   * ===Preconditions===
   * `schedules` must be sorted by start time in ascending order
   *
   * ===Complexity===
   *   - Time: Θ(n) - where n is the schedules size
   *   - Space: Θ(n) - for error messages if any
   */
  def continuous[R]: Rule[R] = schedules =>
    val gaps = schedules.sliding(2).collect {
      case Seq(a, b) if !a.isContinuous(b) => RuleError.Structural(a, b, "non-continuous")
    }
    gaps.toList

  /**
   * Validates that schedules do not overlap in time.
   *
   * ===Preconditions===
   * `schedules` must be sorted by start time in ascending order
   *
   * ===Complexity===
   *   - Time: Θ(n) - where n is the schedules size
   *   - Space: Θ(n) - for error messages if any
   */
  def noConflicts[R]: Rule[R] = schedules =>
    val conflicts = schedules.sliding(2).collect {
      case Seq(a, b) if a.conflictsWith(b) => RuleError.Structural(a, b, s"$a conflicts with $b")
    }
    conflicts.toList

  def validate[R](pred: Schedule[R] => Boolean, errorMsg: Schedule[R] => String): Rule[R] = schedules =>
    val errors = schedules.filterNot(pred).map(s => RuleError.Individual(s, errorMsg(s)))
    errors.toList

  /* ------- combinators ------- */

  /**
   * Combines multiples rules into a single rule, accumulating all errors.
   *
   * ===Monoidal Properties===
   *
   * `allOf` gives `Rule` monoidal properties:
   *   - `allOf()` returns the identity rule
   *   - identity: for all rules, rule == allOf(id, rule) == allOf(rule, id)
   *   - associativity: for all rules, allOf(allOf(rule1, rule2), rule3) == allOf(rule1, allOf(rule2, rule3))
   *
   * Benefits:
   *   - Composability: Complex rules can be composed from simple ones
   *   - Predictability: Rules can be combined in any order (associativity), and all errors aggregated
   *
   * Non commutative: allOf(rule1, rule2) != allOf(rule2, rule1) - errors in the resulting list may have different ordering.
   *
   * ===Complexity===
   *
   * Worst-, average- and best-case time complexities are the same: every schedule is checked against all rules.
   *   - Time: Θ(n*m) - where m = size of schedules and n = size of rules
   */
  def allOf[R](rules: Rule[R]*): Rule[R] = schedules =>
    val errors = rules.flatMap { _.apply(schedules) }
    errors.toList

  /**
   * Combine multiple rules into a single rule, returning errors from the first rule violation only.
   *
   * ===Properties===
   *
   * `firstOf` obeys the identity laws:
   *   - `firstOf()` returns the identity rule
   *   - identity: for all rules, rule == firstOf(id, rule) == firstOf(rule, id)
   *
   * It is non-associative and non-commutative (ordering matters).
   *
   * Benefits:
   *   - Short-circuiting; Stops evaluation as soon as the first rule violation is found
   *   - Deterministic: Earlier rules take precedence over subsequent ones.
   *
   * ===Complexity===
   *
   * The time complexity ranges from Θ(m) to Θ(n*m), where m = size of schedules and n = size of rules. For best-performance,
   * place frequently failing rules first.
   */
  def firstOf[R](rules: Rule[R]*): Rule[R] = schedules =>
    @tailrec
    def loop(remaining: Seq[Rule[R]]): List[RuleError[R]] = remaining match
      case Seq()        => Nil
      case rule +: tail =>
        rule(schedules) match
          case Nil => loop(tail)
          case err => err
    loop(rules)

  /* ------- common business validations ------- */

  def workdaysOnly[R](zoneId: ZoneId): Rule[R] =
    onlyOnDays(Set(MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY), zoneId)

  def onlyOnDays[R](allowedDays: Set[DayOfWeek], zoneId: ZoneId): Rule[R] = schedules =>
    val pred: Schedule[R] => Boolean = s => allowedDays.contains(s.startAt(zoneId).getDayOfWeek())
    val errorMsg: Schedule[R] => String = s => s"$s breaks constraint: only $allowedDays day(s) of the week are allowed"
    validate(pred, errorMsg)(schedules)

  def during[R](allowed: Set[TimeRange], zoneId: ZoneId): Rule[R] = schedules =>
    val pred: Schedule[R] => Boolean = s => allowed.exists(s.isWithinTimeRange(zoneId))
    val errorMsg: Schedule[R] => String = s => s"$s breaks constraint: only $allowed time of the day are allowed"
    validate(pred, errorMsg)(schedules)
