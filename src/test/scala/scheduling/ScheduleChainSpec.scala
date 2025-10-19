package scheduling

import data.interval.Interval.NonEmptyHalfOpenRight
import data.interval.IntervalAlgebra.instances.given
import data.interval.Intervals
import data.interval.testkit.IntervalGens.Factory
import java.time.ZoneOffset.UTC
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*
import scheduling.testkit.ScheduleGen.*
import scheduling.testkit.ScheduleGen.SchedulesContinuityKind.*
import java.time.LocalDate
import java.time.LocalTime
import scheduling.Schedule
import scheduling.testkit.ScheduleContext
import org.scalacheck.Gen
import _root_.testkit.syntax.EitherSyntax.*

class ScheduleChainSpec extends ScalaCheckSuite with ScheduleContext:

  given Factory[Long, NonEmptyHalfOpenRight[Long]] = Intervals.makeNonEmptyHalfOpenRight[Long]

  val isa = Schedules.make("isa", 8, 10)
  val lipe = Schedules.make("lipe", 10, 16)
  val celle = Schedules.make("celle", 16, 20)
  val base = ScheduleChain.makeContinuous(isa, lipe, celle).rightOrFail

  /* Base: isa: [8, 10), lipe: [10, 16), celle: [16, 20)
   * Override: rafa: [6, 8)
   * Merge: rafa: [6, 8), isa: [8, 10), lipe: [10, 16), celle: [16, 20)
   */
  test("mergeStrict prepends overrides that fully precede base schedules"):
    val rafa = Schedules.make("rafa", 6, 8)
    val overrides = ScheduleChain.makeAllowingGaps(rafa).rightOrFail

    val result = ScheduleChain.mergeStrict(base, overrides).rightOrFail

    assertSchedules(result, "rafa" -> (6, 8), "isa" -> (8, 10), "lipe" -> (10, 16), "celle" -> (16, 20))

  /* Base: isa: [8, 10), lipe: [10, 16), celle: [16, 20)
   * Override: rafa: [20, 24)
   * Merge: isa: [8, 10), lipe: [10, 16), celle: [16, 20), rafa: [20, 24)
   */
  test("mergeStrict appends overrides that fully follow base schedules"):
    val rafa = Schedules.make("rafa", 20, 24)
    val overrides = ScheduleChain.makeAllowingGaps(rafa).rightOrFail

    val result = ScheduleChain.mergeStrict(base, overrides).rightOrFail

    assertSchedules(result, "isa" -> (8, 10), "lipe" -> (10, 16), "celle" -> (16, 20), "rafa" -> (20, 24))

  /* Base: isa: [8, 10), lipe: [10, 16), celle: [16, 20)
   * Override: rafa: [8, 10)
   * Merge: rafa: [8, 10), lipe: [10, 16), celle: [16, 20)
   */
  test("mergeStrict replaces base schedules fully overlapped by overrides"):
    val rafa = Schedules.make("rafa", 8, 10)
    val overrides = ScheduleChain.makeAllowingGaps(rafa).rightOrFail

    val result = ScheduleChain.mergeStrict(base, overrides).rightOrFail

    assertSchedules(result, "rafa" -> (8, 10), "lipe" -> (10, 16), "celle" -> (16, 20))

  /* Base: isa: [8, 10), lipe: [10, 16), celle: [16, 20)
   * Override: rafa: [8, 9)
   * Merge: rafa: [8, 9), isa: [9, 10), lipe: [10, 16), celle: [16, 20)
   */
  test("mergeStrict splits base schedules when overrides cover beginning"):
    val rafa = Schedules.make("rafa", 8, 9)
    val overrides = ScheduleChain.makeAllowingGaps(rafa).rightOrFail

    val result = ScheduleChain.mergeStrict(base, overrides).rightOrFail

    assertSchedules(result, "rafa" -> (8, 9), "isa" -> (9, 10), "lipe" -> (10, 16), "celle" -> (16, 20))

  /* Base: isa: [8, 10), lipe: [10, 16), celle: [16, 20)
   * Override: rafa: [9, 10)
   * Merge: isa: [8, 9), rafa: [9, 10), lipe: [10, 16), celle: [16, 20)
   */
  test("mergeStrict splits base schedules when overrides cover ending"):
    val rafa = Schedules.make("rafa", 9, 10)
    val overrides = ScheduleChain.makeAllowingGaps(rafa).rightOrFail

    val result = ScheduleChain.mergeStrict(base, overrides).rightOrFail

    assertSchedules(result, "isa" -> (8, 9), "rafa" -> (9, 10), "lipe" -> (10, 16), "celle" -> (16, 20))

  /* Base: isa: [8, 10), lipe: [10, 16), celle: [16, 20)
   * Override: rafa: [12, 14)
   * Merge: isa: [8, 10), lipe: [10, 12), rafa: [12, 14), lipe: [14, 16), celle: [16, 20)
   */
  test("mergeStrict splits base schedules fully containing overrides"):
    val rafa = Schedules.make("rafa", 12, 14)
    val overrides = ScheduleChain.makeAllowingGaps(rafa).rightOrFail

    val result = ScheduleChain.mergeStrict(base, overrides).rightOrFail

    assertSchedules(result, "isa" -> (8, 10), "lipe" -> (10, 12), "rafa" -> (12, 14), "lipe" -> (14, 16), "celle" -> (16, 20))

  /* Base: isa: [8, 10), lipe: [10, 16), celle: [16, 20)
   * Override: rafa: [9, 12)
   * Merge: isa: [8, 9), rafa: [9, 12), lipe: [12, 16), celle: [16, 20)
   */
  test("mergeStrict splits base schedules when overrides span adjacent intervals"):
    val rafa = Schedules.make("rafa", 9, 12)
    val overrides = ScheduleChain.makeAllowingGaps(rafa).rightOrFail

    val result = ScheduleChain.mergeStrict(base, overrides).rightOrFail

    assertSchedules(result, "isa" -> (8, 9), "rafa" -> (9, 12), "lipe" -> (12, 16), "celle" -> (16, 20))

  /* Base: isa: [8, 10), lipe: [10, 16), celle: [16, 20)
   * Override: rafa: [8, 17)
   * Merge: rafa: [8, 17), celle: [17, 20)
   */
  test("mergeStrict replaces base schedules fully contained by overrides"):
    val rafa = Schedules.make("rafa", 8, 17)
    val overrides = ScheduleChain.makeAllowingGaps(rafa).rightOrFail

    val result = ScheduleChain.mergeStrict(base, overrides).rightOrFail

    assertSchedules(result, "rafa" -> (8, 17), "celle" -> (17, 20))

  /* Base: isa: [8, 10), lipe: [10, 16), celle: [16, 20)
   * Override: rafa: [8, 10), si: [10, 16), drica: [16, 20)
   * Merge: rafa: [8, 10), si: [10, 16), drica: [16, 20)
   */
  test("mergeStrict replaces the entire base chain when overrides match exactly"):
    val rafa = Schedules.make("rafa", 8, 10)
    val si = Schedules.make("si", 10, 16)
    val drica = Schedules.make("drica", 16, 20)
    val overrides = ScheduleChain.makeAllowingGaps(rafa, si, drica).rightOrFail

    val result = ScheduleChain.mergeStrict(base, overrides).rightOrFail

    assertSchedules(result, "rafa" -> (8, 10), "si" -> (10, 16), "drica" -> (16, 20))

  /* Base: isa: [8, 10), lipe: [10, 16), celle: [16, 20)
   * Override: rafa: [8, 10), si: [16, 20]
   * Merge: rafa: [8, 10), lipe: [10, 16), si: [16, 20)
   */
  test("mergeStrict handles multiple overrides with gaps between them"):
    val rafa = Schedules.make("rafa", 8, 10)
    val si = Schedules.make("si", 16, 20)
    val overrides = ScheduleChain.makeAllowingGaps(rafa, si).rightOrFail

    val result = ScheduleChain.mergeStrict(base, overrides).rightOrFail

    assertSchedules(result, "rafa" -> (8, 10), "lipe" -> (10, 16), "si" -> (16, 20))

  /*
   * Base:
   * isa: from 13/10/2025 07:07 to 13/10/2025 15:03, or [1760335664422, 1760364183663)
   * lipe: from 13/10/2025 15:03 to 13/10/2025 17:15, or [1760364183663, 1760372138190)
   *
   * // Overrides
   * rafa: from 13/10/2025 06:56 to 13/10/2025 07:51, or [1760335003093, 1760338319167)
   * si: from 13/10/2025 07:51 to 13/10/2025 10:23, or [1760338319167, 1760347404913)
   *
   * rafa: from 13/10/2025 06:56 to 13/10/2025 07:51, or [1760335003093, 1760338319167)
   * si: from 13/10/2025 07:51 to 13/10/2025 10:23, or [1760338319167, 1760347404913)
   * isa: from 13/10/2025 10:23 to 13/10/2025 15:03, or [1760347404913, 1760364183663)
   *
   * Base:
   * isa:            [-------------------------)
   * lipe:                                     [-----------------)
   *
   * Overrides:
   * rafa:   [-----------)
   * si  :               [---------)
   *
   * Merged:
   * rafa:   [-----------)
   * si  :               [---------)
   * isa:                          [-----------)
   * lipe:                                     [-----------------)
   */
  test("mergeStrict handles multiple adjacent overrides"):
    val isa = Schedules.make("isa", 1760335664422L, 1760364183663L)
    val lipe = Schedules.make("lipe", 1760364183663L, 1760372138190L)
    val base = ScheduleChain.makeContinuous(isa, lipe).rightOrFail
    val rafa = Schedules.make("rafa", 1760335003093L, 1760338319167L)
    val si = Schedules.make("si", 1760338319167L, 1760347404913L)
    val overrides = ScheduleChain.makeAllowingGaps(rafa, si).rightOrFail

    val result = ScheduleChain.mergeStrict(base, overrides).rightOrFail

    assertSchedules(
      result,
      "rafa" -> (rafa.startMillis, rafa.endMillis),
      "si" -> (si.startMillis, si.endMillis),
      "isa" -> (si.endMillis, isa.endMillis),
      "lipe" -> (isa.endMillis, lipe.endMillis)
    )

  // override def scalaCheckInitialSeed = "bXg1XbXngL-83OS_TBLGq6py5Nu5r8MsDznS79CSPWF="

  property("mergeStrict preserves continuity within base window"):
    forAll(schedules, overrides) { case (base, over) =>
      val start = base.schedules.head.startMillis
      val end = base.schedules.last.endMillis
      val trimmedOverrides = ScheduleChain
        .makeAllowingGaps(
          // restricts overrides to the base window preventing gaps in the merged schedules
          over.schedules.filter(_.endMillis >= start).filter(_.startMillis <= end)*
        )
        .rightOrFail

      val result = ScheduleChain.mergeStrict(base, trimmedOverrides).rightOrFail

      Rules.continuous(result.schedules) match
        case Nil    => true
        case errors => fail(s"found discontinuity:\n${errors.mkString("\n")}")
    }

  /*
   * Test strategy:
   *
   * We need a stray override that sits completely outside the base chain's time range, either before of after,
   * so that `mergeStrict` produces a non-continuous schedule chain, and therefore an error.
   *
   * The _start_ and _end_ times of the stray schedule need to adapt to the total base chain duration,
   * ensuring that it doesn't overlap no matter how small or large the span.
   *
   * That's done with a delta variable: `delta = (end - start) * 2`.
   * Multiplying by 2 gives a buffer zone ensuring that the stray falls entirely outside the base span.
   *
   * Example (pre-stray):
   *
   *                                                     <------------------- duration ------------------>
   *
   *        [--------- stray ----------]                 [----------------- base chain -------------------]
   *        ^                          ^                 ^                                                ^
   * (start - delta)            (start - delta/2)        start                                             end
   *
   *
   * Finally, computing start and end based on the max values of base and overrides guarantees that the stray lies fully outside overrides as well.
   * Since `schedules` and `overrides` are independent generators, it is possible that overrides extend the base span.
   * That's also the reason we assert the number of structural errors (non-continuity) is at least (>=) 1.
   */
  property("mergeStrict fails when overrides fall completely outside the base span".ignore):
    forAll(schedules, overrides, Gen.oneOf(true, false)) { case (base, over, pre) =>
      val start = math.max(base.schedules.head.startMillis, over.schedules.head.startMillis)
      val end = math.max(base.schedules.last.endMillis, over.schedules.last.endMillis)
      val delta = (end - start) * 3
      val stray =
        if pre then Schedules.make("pre", start - delta, start - delta / 2)
        else Schedules.make("pos", end + delta / 2, end + delta)
      val overridesWithStray = if pre then stray +: over.schedules else over.schedules :+ stray

      val o = ScheduleChain.makeAllowingGaps(overridesWithStray*).rightOrFail // TODO This seems to be flickering: fix it

      ScheduleChain.mergeStrict(base, o).leftOrFail.size >= 1
    }

  // TODO Fix this: it shouldn't include overrides that cause gaps in the result
  property("mergeStrict includes all override schedules".ignore):
    forAll(schedules, overrides) { case (base, over) =>
      val result = ScheduleChain.mergeStrict(base, over).rightOrFail
      over.schedules.forall(result.schedules.contains(_))
    }

  // TODO Fix this: it shouldn't include overrides that cause gaps in the result
  property("mergeStrict preserves temporal ordering".ignore):
    forAll(schedules, overrides) { case (base, over) =>
      val result = ScheduleChain.mergeStrict(base, over).rightOrFail
      result.schedules.sliding(2).forall { case Seq(a, b) =>
        a.startMillis < b.startMillis
      }
    }

  private val start = LocalDate.of(2025, 10, 13).atTime(LocalTime.MIN)
  private val end = LocalDate.of(2025, 10, 18).atTime(LocalTime.MAX)
  private def schedules = schedulesWithin(start, end, UTC, Set(Continuous))
    .suchThat(_.nonEmpty)
    .map { seq =>
      ScheduleChain.makeContinuous(seq*).rightOrFail
    }
  private def overrides = schedulesWithin(start, end, UTC, Set(Continuous, Gapped))
    .suchThat(_.nonEmpty)
    .map { seq =>
      ScheduleChain.makeAllowingGaps(seq*).rightOrFail
    }

  private def assertSchedules(obtained: ScheduleChain[ResourceId, ?], expected: (ResourceId, (Long, Long))*) =
    assertEquals(obtained.schedules.size, expected.size, s"schedule count mismatch: obtained = ${obtained}")
    obtained.schedules.zip(expected).foreach { case (schedule, (expId, (expStart, expEnd))) =>
      assertEquals(schedule.resource, expId, "resource id mismatch")
      assertEquals(schedule.startMillis, expStart, "schedule start mismatch")
      assertEquals(schedule.endMillis, expEnd, "schedule end mismatch")
    }
