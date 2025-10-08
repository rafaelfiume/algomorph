package scheduling

import java.time.DayOfWeek
import java.time.DayOfWeek.*
import java.time.ZoneId
import scala.annotation.tailrec
import scheduling.Schedule.TimeRange

type Error = String
type Rule[R] = Seq[Schedule[R]] => Either[List[Error], Unit]

object Rules:
  /* ------- primitives ------- */

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
      case Seq(a, b) if !a.isContinuous(b) => s"gap found between $a and $b"
    }
    Either.cond(gaps.isEmpty, right = (), left = gaps.toList)

  /**
   * Validates no conflicts between schedules.
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
      case Seq(a, b) if a.conflictsWith(b) => s"$a conflicts with $b"
    }
    Either.cond(conflicts.isEmpty, right = (), left = conflicts.toList)

  def validate[R](pred: Schedule[R] => Boolean, errorMsg: Schedule[R] => String): Rule[R] = schedules =>
    val errors = schedules.filterNot(pred).map(errorMsg)
    Either.cond(errors.isEmpty, (), errors.toList)

  /* ------- combinators ------- */

  /*
   * Time complexity: Θ(n*m) - where m = size of schedules and n = size of rules
   */
  def allOf[R](rules: Rule[R]*): Rule[R] = schedules =>
    val errors = rules.flatMap { rule => rule(schedules).left.getOrElse(Nil) }
    Either.cond(errors.isEmpty, (), errors.toList)

  def firstFailure[R](rules: Rule[R]*): Rule[R] = schedules =>
    @tailrec
    def loop(remaining: Seq[Rule[R]]): Either[List[Error], Unit] = remaining match
      case Nil         => Right(())
      case rule :: rem =>
        rule(schedules) match
          case Right(_)      => loop(rem)
          case err @ Left(_) => err
    loop(rules)

  /* ------- common business validations ------- */

  def workdaysOnly[R](zoneId: ZoneId = ZoneId.systemDefault()): Rule[R] =
    onlyOnDays(Set(MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY), zoneId)

  def onlyOnDays[R](allowedDays: Set[DayOfWeek], zoneId: ZoneId): Rule[R] = schedules =>
    val pred: Schedule[R] => Boolean = s => allowedDays.contains(s.startAt(zoneId).getDayOfWeek())
    val errorMsg: Schedule[R] => String = s => s"$s violates constraint: only $allowedDays day(s) of the week are allowed"
    validate(pred, errorMsg)(schedules)

  def during[R](allowed: Set[TimeRange], zoneId: ZoneId): Rule[R] = schedules =>
    val pred: Schedule[R] => Boolean = s => allowed.exists(s.isWithinTimeRange(zoneId))
    val errorMsg: Schedule[R] => String = s => s"$s violates constraint: only $allowed time of the day are allowed"
    validate(pred, errorMsg)(schedules)
