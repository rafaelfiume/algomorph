package data.interval.testkit

import java.time.Instant

/*
 * Conversion from generic type `T` to `Long`.
 * 
 * `Schedules.make` smart constructors only accept `Long` or `Instant`.
 * Since interval and schedule generators with a generic type `T`,
 * we must convert `T` -> `Long` when deriving `Schedule`'s from `Interval` generators.
 */
trait ToEpoch[T]:
  def toEpochMillis(t: T): Long

object ToEpoch:
  given ToEpoch[Int]:
    def toEpochMillis(t: Int) = t

  given ToEpoch[Long]:
    def toEpochMillis(t: Long) = t

  given ToEpoch[Instant]:
    def toEpochMillis(t: Instant) = t.toEpochMilli
