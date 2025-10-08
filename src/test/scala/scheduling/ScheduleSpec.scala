package scheduling

import data.interval.Interval.NonEmptyHalfOpenRight
import data.interval.IntervalAlgebra.instances.given
import data.interval.Intervals
import data.interval.testkit.IntervalGens.Factory
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.*
import org.scalacheck.ShrinkLowPriority
import scheduling.testkit.ScheduleContext
import scheduling.testkit.ScheduleGen.*
import scheduling.Schedule.TimeRange

class ScheduleSpec extends ScalaCheckSuite with ShrinkLowPriority with ScheduleContext:

  given Factory[Long, NonEmptyHalfOpenRight[Long]] = Intervals.makeNonEmptyHalfOpenRight[Long]

  property("isWithinTimeRange succeeds for all schedules in the allowed time range"):
    forAll(schedulesWithinTimeRange) { case (schedules, allowedTimeRange, _) =>
      schedules.forall { _.isWithinTimeRange(utc)(allowedTimeRange.start, allowedTimeRange.end) }
    }

  property("isWithinTimeRange fails for all schedules outside the allowed time range"):
    forAll(schedulesWithinTimeRange) { case (schedules, _, disallowedRange) =>
      schedules.forall { !_.isWithinTimeRange(utc)(disallowedRange.start, disallowedRange.end) }
    }

  private def schedulesWithinTimeRange: Gen[(Seq[Schedule[ResourceId]], TimeRange, TimeRange)] =
    for
      allowedTimeRange <- Gen.oneOf(timeRanges)
      schedules <- schedulesWithin(frozenDate, Set(allowedTimeRange), utc)
      disallowedRange <- Gen.oneOf(outsideAllowance(timeRanges))
    yield (schedules, allowedTimeRange, disallowedRange)
