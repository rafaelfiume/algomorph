package scheduling

import munit.ScalaCheckSuite
// import scheduling.Rules
import org.scalacheck.Gen
import data.interval.Interval
import data.interval.testkit.IntervalGens.*
import data.interval.Intervals
import data.interval.IntervalAlgebra.instances.given
// import data.interval.BoundedAlgebra.instances.given // use custom BoundedAlgebra to restrict temporal interval
import org.scalacheck.Prop.*
import data.interval.IntervalAlgebra
import data.interval.BoundedAlgebra
import java.time.Instant
import data.interval.testkit.ToEpoch
import org.scalacheck.ShrinkLowPriority
import java.time.LocalDate
import java.time.LocalTime
import java.time.ZoneId
import scheduling.Schedule.TimeRange

class RulesSpec extends ScalaCheckSuite with ShrinkLowPriority:

  /*
   * The Challenge: Testing rules operating in calendar time, such as checking a schedule is within one of the defined `Set[TimeRange]`, expressing daily recurring windows.
   *
   * - `Schedule`s are built upon continuous numeric intervals (epoch millis), great for arithmetic reasoning, but with no notion of day boundaries or local time
   * - Thus, testing "within working hours" or "one of the allowed time ranges" is not straightforward.
   *
     * The Solution:
   *
     * Leverage existing IntervalGens machinery with a custom `BoundedAlgebra`` to generate a sequence of `Schedule` within (and out of) allowed time ranges.
   */
  object TemporalBounds:
    def dayBound(date: LocalDate, start: LocalTime, end: LocalTime, zoneId: ZoneId): BoundedAlgebra[Long] =
      require(start.isBefore(end))
      new BoundedAlgebra[Long]:
        override def infimum: Long = date.atTime(start).atZone(zoneId).toInstant().toEpochMilli()
        override def supremum: Long = date.atTime(end).atZone(zoneId).toInstant().toEpochMilli()

    def modernDaysBound(): BoundedAlgebra[Long] = new BoundedAlgebra[Long]:
      override def infimum: Long = Instant.parse("2018-01-01T09:00:00Z").toEpochMilli()
      override def supremum: Long = Instant.parse("2030-12-31T17:00:00Z").toEpochMilli()

  /*
   * Note:
   *
   * checkScheduleChainProperties(Intervals.makeHalfOpenRight[Instant]) doesn't compile
   * because `Interval` generators require an `Integral` instance, which cannot sensibly be defined for `Instant`.
   *
   * Using `Long` (epoch millis) is sufficient for verifying scheduling behaviour,
   * since it provides a precise and arithmetic-friendly temporal representation.
   *
   * In case there is the need to check scheduling properties based on `Instant`, a possible solution
   * would be to define a custom type class providing the minimal arithmetic operations required (`+`, `-`)
   * to replace `Integral`.
   */

  // Structural rules (time-boundary agnostic): test gaps, conflicts, continuity
  checkContinuityProperties(Intervals.makeNonEmptyHalfOpenRight[Long], TemporalBounds.modernDaysBound())

  // Time-range rules (calendar-aware): e.g. check schedules falls within business-hours only
  checkTimeRangeRules(Intervals.makeNonEmptyHalfOpenRight[Long])

  private def checkContinuityProperties[T: Integral: ToEpoch: Gen.Choose, I <: Interval[T]](
    factory: Factory[T, I],
    temporalBound: BoundedAlgebra[T]
  )(using
    alg: IntervalAlgebra[T, I]
  ): Unit =
    given BoundedAlgebra[T] = temporalBound
    given Factory[T, I] = factory

    property("Rules.continuous succeeds for back-to-back schedules"):
      forAll(continuous) { schedules =>
        Rules.continuous(schedules).isRight
      }

    property("Rules.continuous fails for non-continuos schedules"):
      forAll(Gen.oneOf(gapped, conflicting)) { schedules =>
        val result = Rules.continuous(schedules)
        result.isLeft && result.left.exists(_.size == schedules.size - 1)
      }

    property("Rules.noConflicts rule succeeds for non-conflicting schedules"):
      forAll(Gen.oneOf(continuous, gapped)) { schedules =>
        Rules.noConflicts(schedules).isRight
      }

    property("Rules.noConflicts rule fails for conflicting schedules"):
      forAll(conflicting) { schedules =>
        val result = Rules.noConflicts(schedules)
        result.isLeft && result.left.exists(_.size == schedules.size - 1)
      }

  /*
   * These tests use a single-day temporal bound (`TemporalBound.dayBound`).
   *
   * Testing single-day bounded schedules is representative of Rules.during behaviour in multi-day scenarios because:
   *
   *   - `Rules`` tests work as integration checks, verifying that primitive components (e.g. `Schedules.isWithinTimeRange`) compose correctly
   *   - `isWithinTimeRange` handles cross-midnight cases (e.g. 22:00 -> 02:00) and other recurring time-range semantics
   *   - Time-range rules are defined in terms of local times only, independent of specific calendar dates.
   */
  private def checkTimeRangeRules[I <: Interval[Long]](factory: Factory[Long, I])(using alg: IntervalAlgebra[Long, I]): Unit =
    val zoneId = ZoneId.of("UTC")
    val frozenDate = LocalDate.of(2025, 10, 10)
    given Factory[Long, I] = factory

    property("Rules.during succeeds for schedules within the allowed time ranges"):
      // Idea: Generate allowed time range using intervals gens
      val allowed: Set[TimeRange] = Set(
        (start = LocalTime.of(10, 0), end = LocalTime.of(12, 0)), // 10:00 -> 12:00
        (start = LocalTime.of(14, 0), end = LocalTime.of(17, 0)) // 14:00 -> 17:00
      )
      forAll(schedulesWithinTimeRange(frozenDate, allowed, zoneId)) { schedules =>
        Rules.during(allowed, zoneId)(schedules).isRight
      }

    property("Rules.during fails for schedules with disallowed time ranges"):
      val allowed: Set[TimeRange] = Set(
        (start = LocalTime.of(10, 0), end = LocalTime.of(12, 0)),
        (start = LocalTime.of(14, 0), end = LocalTime.of(17, 0))
      )
      forAll(schedulesWithinTimeRange(frozenDate, outsideAllowance(allowed), zoneId)) { schedules =>
        val result = Rules.during(allowed, zoneId)(schedules)
        result.isLeft && result.left.exists { _.size == schedules.size }
      }

    def outsideAllowance(allowed: Set[TimeRange]): Set[TimeRange] =
      val sorted = allowed.toList.sortBy(_.start)
      require(sorted.sliding(2).forall {
        case List(a, b) => a.end.isBefore(b.start)
        case _          => true
      })
      val begin = LocalTime.MIN
      val finish = LocalTime.MAX.minusNanos(999_999)
      (begin :: sorted.map(_.end)).zip(sorted.map(_.start) ++ List(finish)).toSet

  type ResourceId = String

  def continuous[T: Integral: BoundedAlgebra: ToEpoch: Gen.Choose, I <: Interval[T]](using
    IntervalAlgebra[T, I],
    Factory[T, I]
  ): Gen[Seq[Schedule[ResourceId]]] = scheduleSeq(idGen, adjacentIntervalChain)

  def gapped[T: Integral: BoundedAlgebra: ToEpoch: Gen.Choose, I <: Interval[T]](using
    Factory[T, I]
  ): Gen[Seq[Schedule[ResourceId]]] = scheduleSeq(idGen, disjointIntervalChain)

  def conflicting[T: Integral: BoundedAlgebra: ToEpoch: Gen.Choose, I <: Interval[T]](using
    Factory[T, I]
  ): Gen[Seq[Schedule[ResourceId]]] = scheduleSeq(idGen, intersectingIntervalChain)

  def scheduleSeq[T](ids: Gen[ResourceId], intervalSeq: Gen[Seq[Interval[T]]])(using
    conv: ToEpoch[T]
  ): Gen[Seq[Schedule[ResourceId]]] =
    intervalSeq.flatMap { intervals =>
      Gen.sequence { // 2. Gen.sequence to the rescue: return Gen[Seq[Schedule[ResourceId]]]
        intervals.map { interval => // 1. The following returns: Seq[Gen[Schedule[ResourceId]]]
          ids.flatMap { id =>
            Schedules.make(id, conv.toEpochMillis(interval.start), conv.toEpochMillis(interval.end))
          }
        }
      }
    }

  def schedulesWithinTimeRange[I <: Interval[Long]](date: LocalDate, allowed: Set[TimeRange], zoneId: ZoneId)(using
    Factory[Long, I],
    IntervalAlgebra[Long, I]
  ): Gen[Seq[Schedule[ResourceId]]] =
    scheduleSeq(idGen, withinTimeRange(date, allowed, zoneId))

  def withinTimeRange[I <: Interval[Long]](date: LocalDate, allowed: Set[TimeRange], zoneId: ZoneId)(using
    Factory[Long, I],
    IntervalAlgebra[Long, I]
  ): Gen[Seq[I]] =
    Gen.oneOf(allowed).flatMap { timeRange =>
      given BoundedAlgebra[Long] = TemporalBounds.dayBound(date, timeRange.start, timeRange.end, zoneId)
      Gen.oneOf(adjacentIntervalChain[Long, I], disjointIntervalChain[Long, I], intersectingIntervalChain[Long, I])
    }

  private def idGen: Gen[ResourceId] = Gen.uuid.map(_.toString().take(8))
