package scheduling.testkit

import data.interval.BoundedAlgebra
import data.interval.Interval
import data.interval.IntervalAlgebra
import data.interval.testkit.IntervalGens.*
import data.interval.testkit.ToEpoch
import org.scalacheck.Gen
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.ZoneId
import scheduling.Schedule
import scheduling.Schedule.TimeRange
import scheduling.Schedules

object ScheduleGen:

  type ResourceId = String

  def schedules[T: Integral: BoundedAlgebra: ToEpoch: Gen.Choose, I <: Interval[T]](using
    IntervalAlgebra[T, I],
    Factory[T, I]
  ): Gen[Seq[Schedule[ResourceId]]] = Gen.oneOf(continuous, gapped, conflicting)

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

  def schedulesWithin[I <: Interval[Long]](date: LocalDate, allowed: Set[TimeRange], zoneId: ZoneId)(using
    Factory[Long, I],
    IntervalAlgebra[Long, I]
  ): Gen[Seq[Schedule[ResourceId]]] =
    val withinTimeRange = Gen.oneOf(allowed).flatMap { timeRange =>
      withinDatesRange(date.atTime(timeRange.start), date.atTime(timeRange.end), zoneId)
    }
    scheduleSeq(idGen, withinTimeRange)

  def schedulesWithin[I <: Interval[Long]](start: LocalDateTime, end: LocalDateTime, zoneId: ZoneId)(using
    Factory[Long, I],
    IntervalAlgebra[Long, I]
  ): Gen[Seq[Schedule[ResourceId]]] = scheduleSeq(idGen, withinDatesRange(start, end, zoneId))

  private def withinDatesRange[I <: Interval[Long]](start: LocalDateTime, end: LocalDateTime, zoneId: ZoneId)(using
    Factory[Long, I],
    IntervalAlgebra[Long, I]
  ): Gen[Seq[I]] =
    given BoundedAlgebra[Long] = TemporalBounds.daysBound(start, end, zoneId)
    Gen.oneOf(adjacentIntervalChain[Long, I], disjointIntervalChain[Long, I], intersectingIntervalChain[Long, I])

  private def idGen: Gen[ResourceId] = Gen.uuid.map(_.toString().take(8))
