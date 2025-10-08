package scheduling.testkit

import java.time.LocalDate
import java.time.LocalTime
import java.time.ZoneId
import scheduling.Schedule.TimeRange

trait ScheduleContext:
  val utc = ZoneId.of("UTC")

  val frozenDate = LocalDate.of(2025, 10, 10)

  // Idea: Generate allowed time range using intervals gens
  val timeRanges: Set[TimeRange] = Set(
    (start = LocalTime.of(10, 0), end = LocalTime.of(12, 0)), // 10:00 -> 12:00
    (start = LocalTime.of(14, 0), end = LocalTime.of(17, 0)) // 14:00 -> 17:00
  )

  def outsideAllowance(allowed: Set[TimeRange]): Set[TimeRange] =
    val sorted = allowed.toList.sortBy(_.start)
    require(sorted.sliding(2).forall {
      case List(a, b) => a.end.isBefore(b.start)
      case _          => true
    })
    val begin = LocalTime.MIN
    val finish = LocalTime.MAX.minusNanos(999_999)
    (begin :: sorted.map(_.end)).zip(sorted.map(_.start) ++ List(finish)).toSet
