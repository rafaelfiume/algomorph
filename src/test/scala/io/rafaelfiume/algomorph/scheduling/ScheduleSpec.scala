package io.rafaelfiume.algomorph.scheduling

import io.rafaelfiume.algomorph.data.interval.Interval.NonEmptyHalfOpenRight
import io.rafaelfiume.algomorph.data.interval.IntervalAlgebra.instances.given
import io.rafaelfiume.algomorph.data.interval.Intervals
import io.rafaelfiume.algomorph.data.interval.testkit.IntervalGens.Factory
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.*
import org.scalacheck.ShrinkLowPriority
import io.rafaelfiume.algomorph.scheduling.testkit.ScheduleContext
import io.rafaelfiume.algomorph.scheduling.testkit.ScheduleGen.*
import io.rafaelfiume.algomorph.scheduling.Schedule.TimeRange

class ScheduleSpec extends ScalaCheckSuite with ShrinkLowPriority with ScheduleContext:

  given Factory[Long, NonEmptyHalfOpenRight[Long]] = Intervals.makeNonEmptyHalfOpenRight[Long]

  property("isWithinTimeRange succeeds for all schedules in the allowed time range"):
    forAll(schedulesWithAllowedTimeRange) { case (schedules, allowedTimeRange, _) =>
      schedules.forall { _.isWithinTimeRange(utc)(allowedTimeRange.start, allowedTimeRange.end) }
    }

  property("isWithinTimeRange fails for all schedules outside the allowed time range"):
    forAll(schedulesWithAllowedTimeRange) { case (schedules, _, disallowedRange) =>
      schedules.forall { !_.isWithinTimeRange(utc)(disallowedRange.start, disallowedRange.end) }
    }

  private def schedulesWithAllowedTimeRange: Gen[(Seq[Schedule[ResourceId]], TimeRange, TimeRange)] =
    for
      allowedTimeRange <- Gen.oneOf(timeRanges)
      schedules <- schedulesWithinTimeRange(frozenDate, Set(allowedTimeRange), utc)
      disallowedRange <- Gen.oneOf(outsideAllowance(timeRanges))
    yield (schedules, allowedTimeRange, disallowedRange)
