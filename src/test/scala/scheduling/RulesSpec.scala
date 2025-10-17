package scheduling

import data.interval.BoundedAlgebra
import data.interval.Interval
import data.interval.Intervals
import data.interval.IntervalAlgebra.instances.given
import data.interval.IntervalAlgebra
import data.interval.testkit.IntervalGens.*
import data.interval.testkit.ToEpoch
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.*
import org.scalacheck.ShrinkLowPriority
import java.time.LocalDate
import java.time.LocalTime
import scheduling.Rules.allOf
import scheduling.testkit.ScheduleContext
import scheduling.testkit.ScheduleGen.*
import scheduling.testkit.TemporalBounds

class RulesSpec extends ScalaCheckSuite with ShrinkLowPriority with ScheduleContext:

  /*
   * Structural rules (time-boundary agnostic):
   * Test gaps, conflicts, continuity and other relationships between schedules.
   *
   * Note:
   *
   * checkStructuralProperties(Intervals.makeHalfOpenRight[Instant]) doesn't compile
   * because `Interval` generators require an `Integral` instance, which cannot sensibly be defined for `Instant`.
   *
   * Using `Long` (epoch millis) is sufficient for verifying scheduling behaviour,
   * since it provides a precise and arithmetic-friendly temporal representation.
   *
   * In case there is the need to check scheduling properties based on `Instant`, a possible solution
   * would be to define a custom type class providing the minimal arithmetic operations required (`+`, `-`)
   * to replace `Integral`.
   */
  checkStructuralProperties(Intervals.makeNonEmptyHalfOpenRight[Long], TemporalBounds.modernDaysBound())

  /*
   * Time-range rules (calendar-aware):
   * Ensures that each schedule comply with calendar-based constraints.
   *
   * Note:
   *
   * These tests use a single-day temporal bound (@see `TemporalBound.dayBounds`).
   *
   * Testing single-day bounded schedules is representative of Rules.during behaviour in multi-day scenarios because:
   *   - `Rules`` tests work as integration checks, verifying that primitive components (e.g. `Schedules.isWithinTimeRange`) compose correctly
   *   - `isWithinTimeRange` handles cross-midnight cases (e.g. 22:00 -> 02:00) and other recurring time-range semantics
   *   - Time-range rules are defined in terms of local times only, independent of specific calendar dates.
   */
  checkDateAndTimeRangeRules(Intervals.makeNonEmptyHalfOpenRight[Long])

  /*
   * Combinators rules:
   * Checks that combinators obey algebraic laws ensuring predictability and composability.
   */
  checkCombinators(Intervals.makeNonEmptyHalfOpenRight[Long], TemporalBounds.modernDaysBound())

  private def checkStructuralProperties[T: Integral: ToEpoch: Gen.Choose, I <: Interval[T]](
    factory: Factory[T, I],
    temporalBound: BoundedAlgebra[T]
  )(using
    alg: IntervalAlgebra[T, I]
  ): Unit =
    given BoundedAlgebra[T] = temporalBound
    given Factory[T, I] = factory

    property("Rules.continuous succeeds for back-to-back schedules"):
      forAll(continuous) { schedules =>
        Rules.continuous(schedules).isEmpty
      }

    property("Rules.continuous fails for non-continuos schedules"):
      forAll(Gen.oneOf(gapped, conflicting)) { schedules =>
        assertStructuralErrors(Rules.continuous, schedules)
      }

    property("Rules.noConflicts rule succeeds for non-conflicting schedules"):
      forAll(Gen.oneOf(continuous, gapped)) { schedules =>
        Rules.noConflicts(schedules).isEmpty
      }

    property("Rules.noConflicts rule fails for conflicting schedules"):
      forAll(conflicting) { schedules =>
        assertStructuralErrors(Rules.noConflicts, schedules)
      }

  private def checkDateAndTimeRangeRules[I <: Interval[Long]](factory: Factory[Long, I])(using
    alg: IntervalAlgebra[Long, I]
  ): Unit =
    given Factory[Long, I] = factory

    property("Rules.workdaysOnly succeeds when schedules fall on weekdays"):
      val monday = LocalDate.of(2025, 10, 6).atTime(LocalTime.MIN)
      val friday = LocalDate.of(2025, 10, 10).atTime(LocalTime.MAX.minusNanos(999_999))
      forAll(schedulesWithin(monday, friday, utc)) { schedules =>
        Rules.workdaysOnly(utc)(schedules).isEmpty
      }

    property("Rules.workdaysOnly fails when schedules fall on weekends"):
      val saturday = LocalDate.of(2025, 10, 11).atTime(LocalTime.MIN)
      val sunday = LocalDate.of(2025, 10, 12).atTime(LocalTime.MAX.minusNanos(999_999))
      forAll(schedulesWithin(saturday, sunday, utc)) { schedules =>
        assertIndividualErrors(Rules.workdaysOnly(utc), schedules)
      }

    property("Rules.during succeeds for schedules within the allowed time ranges"):
      forAll(schedulesWithinTimeRange(frozenDate, timeRanges, utc)) { schedules =>
        Rules.during(timeRanges, utc)(schedules).isEmpty
      }

    property("Rules.during fails for schedules outside the allowed time ranges"):
      forAll(schedulesWithinTimeRange(frozenDate, outsideAllowance(timeRanges), utc)) { schedules =>
        assertIndividualErrors(Rules.during(timeRanges, utc), schedules)
      }

  private def checkCombinators[T: Integral: ToEpoch: Gen.Choose, I <: Interval[T]](
    factory: Factory[T, I],
    temporalBound: BoundedAlgebra[T]
  )(using alg: IntervalAlgebra[T, I]): Unit =
    given Factory[T, I] = factory
    given BoundedAlgebra[T] = temporalBound

    val identityRule: Rule[ResourceId] = _ => Nil
    val alwaysOk = identityRule
    val perScheduleError: Rule[ResourceId] = s => List(RuleError.Individual(s.head, "one error"))
    val structuralError: Rule[ResourceId] = s => List(RuleError.Structural(s.head, s.last, "another error"))
    val rules: Gen[Rule[ResourceId]] =
      Gen.oneOf(alwaysOk, perScheduleError, structuralError, Rules.continuous, Rules.noConflicts)
    val failingRules: Gen[Seq[Rule[ResourceId]]] =
      Gen.someOf(List(perScheduleError, structuralError, Rules.continuous, Rules.noConflicts)).map(_.toSeq).suchThat(_.nonEmpty)

    property("Rules.allOf succeeds when all rules succeed"):
      forAll(schedules) { schedules =>
        Rules.allOf(alwaysOk, alwaysOk, alwaysOk)(schedules).isEmpty
      }

    property("Rules.allOf aggregates errors of failing rules"):
      forAll(schedules) { schedules =>
        Rules.allOf(perScheduleError, alwaysOk, structuralError)(schedules) ==
          perScheduleError(schedules) ++ alwaysOk(schedules) ++ structuralError(schedules)
      }

    property("Rules.allOf has identity"):
      forAll(schedules, rules) { case (schedules, rule) =>
        val left = Rules.allOf(identityRule, rule)(schedules)
        val right = Rules.allOf(rule, identityRule)(schedules)

        rule(schedules) == left && left == right
      }

    property("Rules.allOf is associative"):
      forAll(schedules) { schedules =>
        allOf(allOf(perScheduleError, identityRule), structuralError)(schedules) ==
          allOf(perScheduleError, allOf(identityRule, structuralError))(schedules)
      }

    property("Rules.allOf() yields the identity rule"):
      forAll(schedules) { schedules =>
        Rules.allOf()(schedules) == identityRule(schedules)
      }

    property("Rules.firstOf has identity"):
      forAll(schedules, rules) { case (schedules, rule) =>
        val left = Rules.firstOf(identityRule, rule)(schedules)
        val right = Rules.firstOf(rule, identityRule)(schedules)

        rule(schedules) == left && left == right
      }

    property("Rules.firstOf short-circuits on the first failing rule"):
      forAll(conflicting, failingRules) { case (schedules, rules) =>
        Rules.firstOf(rules*)(schedules) == rules.head.apply(schedules)
      }

  /**
   * Checks structural-rule validations.
   *
   * This function supports a property-based testing strategy where input validity (wether invariants hold or not) is guaranteed
   * by generators themselves.
   *
   * Generators are responsible for producing schedules that either satisfy or intentionally break structural invariants (e.g.
   * continuity, no conflicts, no gaps).
   *
   * Assertions remain minimal, verifying only the expected outcome: the presence or absence of structural errors across the
   * entire schedule sequence.
   *
   * In this context, every pair of consecutive schedules - `(a_1, a_2), (a_2, a_3), ... , (a_n-1, a_n)` - is expected to produce
   * a corresponding `RuleError.Structural`. If this assumption does not hold for a given input (for example, if only a few pairs
   * conflict), this function should **not** be used.
   *
   * @see
   *   [[assertIndividualErrors]]
   */
  private def assertStructuralErrors(validation: Rule[ResourceId], input: Seq[Schedule[ResourceId]]): Unit =
    val errors = validation(input)
    assert(errors.nonEmpty, s"expected structural errors, but got none for schedules = $input")

    val structuralErrors = errors.collect { case e: RuleError.Structural[ResourceId] => e }

    input.sliding(2).foreach {
      case Seq(schA, schB) =>
        val found = structuralErrors.exists { e => e.a == schA && e.b == schB }
        assert(found, s"expected structural error between $schA and $schB, but none found\nerrors = $errors")

      case _ => fail("expected pairwise comparisons")
    }

  /**
   * Every schedule is expected to produce a corresponding `RuleError.Individual`. If this assumption does not hold for a given
   * input (for example, if only a few pairs conflict), this function should **not** be used.
   *
   * @see
   *   [[assertStructuralErrors]]
   */
  private def assertIndividualErrors(validation: Rule[ResourceId], input: Seq[Schedule[ResourceId]]): Unit =
    val errors = validation(input)
    assert(errors.nonEmpty, s"expected errors, but got none for schedules = $input")

    val individualErrors = errors.collect { case e: RuleError.Individual[ResourceId] => e }

    input.foreach { sch =>
      val found = individualErrors.exists(_.a == sch)
      assert(found, s"expected error in $sch, but none found\nerrors = $errors")
    }
