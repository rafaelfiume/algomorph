package scheduling.testkit

import data.interval.BoundedAlgebra
import java.time.LocalDateTime
import java.time.Instant
import java.time.ZoneId

/**
 * The Challenge: Testing rules operating in calendar time, such as checking a schedule is within one of the defined
 * `Set[TimeRange]`, expressing daily recurring windows.
 *
 *   - `Schedule`s are built upon continuous numeric intervals (epoch millis), great for arithmetic reasoning, but with no notion
 *     of day boundaries or local time
 *   - Thus, testing "within working hours" or "one of the allowed time ranges" is not straightforward.
 *
 * The Solution:
 *
 * Leverage existing IntervalGens machinery with a custom `BoundedAlgebra`` to generate a sequence of `Schedule` within (and out
 * of) allowed time ranges.
 */
object TemporalBounds:
  def daysBound(start: LocalDateTime, end: LocalDateTime, zoneId: ZoneId): BoundedAlgebra[Long] =
    require(start.isBefore(end))
    new BoundedAlgebra[Long]:
      override def infimum: Long = start.atZone(zoneId).toInstant().toEpochMilli()
      override def supremum: Long = end.atZone(zoneId).toInstant().toEpochMilli()

  def modernDaysBound(): BoundedAlgebra[Long] = new BoundedAlgebra[Long]:
    override def infimum: Long = Instant.parse("2018-01-01T09:00:00Z").toEpochMilli()
    override def supremum: Long = Instant.parse("2030-12-31T17:00:00Z").toEpochMilli()
