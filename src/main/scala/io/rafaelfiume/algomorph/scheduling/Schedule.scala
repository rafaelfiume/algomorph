package io.rafaelfiume.algomorph.scheduling

import io.rafaelfiume.algomorph.data.interval.Interval.NonEmptyHalfOpenRight
import java.time.Instant
import io.rafaelfiume.algomorph.data.interval.Intervals
import java.time.ZoneId
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.time.LocalTime

private type TemporalInterval = NonEmptyHalfOpenRight[Long]

sealed abstract case class Schedule[R](resource: R, private val interval: TemporalInterval):
  override def toString: String =
    val startTime = Instant.ofEpochMilli(interval.start)
    val endTime = Instant.ofEpochMilli(interval.end)
    val formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm")
    s"$resource: from ${formatter.format(startTime.atZone(ZoneId.systemDefault))} to ${formatter.format(endTime.atZone(ZoneId.systemDefault))}, or $interval"

object Schedule:
  import io.rafaelfiume.algomorph.data.interval.IntervalAlgebra.syntax.*
  import io.rafaelfiume.algomorph.data.interval.IntervalAlgebra.instances.given

  type TimeRange = (start: LocalTime, end: LocalTime)

  extension [R](schedule: Schedule[R])
    def startMillis: Long = schedule.interval.start
    def endMillis: Long = schedule.interval.end

    def startInstant: Instant = Instant.ofEpochMilli(schedule.interval.start)
    def endInstant: Instant = Instant.ofEpochMilli(schedule.interval.end)

    def startAt(zoneId: ZoneId): ZonedDateTime = startInstant.atZone(zoneId)
    def endAt(zoneId: ZoneId): ZonedDateTime = endInstant.atZone(zoneId)

    def isWithinTimeRange(zoneId: ZoneId)(range: TimeRange): Boolean =
      val startAt = schedule.startAt(zoneId).toLocalTime()
      val endAt = schedule.endAt(zoneId).toLocalTime()
      // same day, e.g (09:00, 13:00)
      if range.start.isBefore(range.end) then !startAt.isBefore(range.start) && !endAt.isAfter(range.end)
      else // overnight range, e.g (23:00, 2:00)
        !endAt.isBefore(range.start) && !startAt.isAfter(range.end)

    def conflictsWith(other: Schedule[R]): Boolean = schedule.interval.intersects(other.interval)
    def isContinuous(other: Schedule[R]): Boolean = schedule.interval.isAdjacent(other.interval)

object Schedules:

  /**
   * Usage:
   * {{{
   * val meeting = Schedules.make("room 1", Instant.now(), Instant.now().plusSeconds(3600))
   * }}}
   */
  def make[R](resource: R, start: Instant, end: Instant): Schedule[R] =
    make(resource, start.toEpochMilli(), end.toEpochMilli())

  /**
   * Negative epoch millis are valid and represent date before the Unix epoch (1st January 1970).
   *
   * Example: start time at -851088960 = December 22, 1969, 04:35:04 UTC.
   */
  def make[R](resource: R, start: Long, end: Long): Schedule[R] =
    new Schedule[R](resource, Intervals.makeNonEmptyHalfOpenRight(start, end)) {}
