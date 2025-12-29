package io.rafaelfiume.algomorph.scheduling.testkit

import io.rafaelfiume.algomorph.data.interval.{BoundedAlgebra, Interval, IntervalAlgebra}
import io.rafaelfiume.algomorph.data.interval.testkit.IntervalGens.*
import io.rafaelfiume.algomorph.data.interval.testkit.ToEpoch
import io.rafaelfiume.algomorph.scheduling.{Schedule, Schedules}
import io.rafaelfiume.algomorph.scheduling.Schedule.TimeRange
import io.rafaelfiume.algomorph.scheduling.testkit.ScheduleGen.SchedulesContinuityKind.*
import org.scalacheck.Gen

import java.time.{LocalDate, LocalDateTime, ZoneId}

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

  enum SchedulesContinuityKind:
    case Continuous
    case Gapped
    case Conflicting

  def schedulesWithinTimeRange[I <: Interval[Long]](
    date: LocalDate,
    allowed: Set[TimeRange],
    zoneId: ZoneId,
    allowedContinuityKind: Set[SchedulesContinuityKind] = Set(Continuous, Gapped, Conflicting)
  )(using
    Factory[Long, I],
    IntervalAlgebra[Long, I]
  ): Gen[Seq[Schedule[ResourceId]]] =
    val withinTimeRange = Gen.oneOf(allowed).flatMap { timeRange =>
      withinDatesRange(date.atTime(timeRange.start), date.atTime(timeRange.end), zoneId, allowedContinuityKind)
    }
    scheduleSeq(idGen, withinTimeRange)

  def schedulesWithin[I <: Interval[Long]](
    start: LocalDateTime,
    end: LocalDateTime,
    zoneId: ZoneId,
    allowedContinuityKind: Set[SchedulesContinuityKind] = Set(Continuous, Gapped, Conflicting)
  )(using
    Factory[Long, I],
    IntervalAlgebra[Long, I]
  ): Gen[Seq[Schedule[ResourceId]]] = scheduleSeq(idGen, withinDatesRange(start, end, zoneId, allowedContinuityKind))

  private def withinDatesRange[I <: Interval[Long]](
    start: LocalDateTime,
    end: LocalDateTime,
    zoneId: ZoneId,
    allowedContinuityKind: Set[SchedulesContinuityKind]
  )(using
    Factory[Long, I],
    IntervalAlgebra[Long, I]
  ): Gen[Seq[I]] =
    given BoundedAlgebra[Long] = TemporalBounds.daysBound(start, end, zoneId)

    val intervalChainGens: List[Gen[Seq[I]]] = allowedContinuityKind.toList.collect {
      case Continuous  => adjacentIntervalChain[Long, I]
      case Gapped      => disjointIntervalChain[Long, I]
      case Conflicting => intersectingIntervalChain[Long, I]
    }

    intervalChainGens match
      case Nil              => throw new IllegalArgumentException("you must specify a continuity kind")
      case g1 :: Nil        => g1
      case g1 :: g2 :: tail => Gen.oneOf(g1, g2, tail*)

  private def idGen: Gen[ResourceId] = Gen.uuid.map(_.toString().take(8))
