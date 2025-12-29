package io.rafaelfiume.algomorph.scheduling

import java.time.{Instant, LocalDateTime, ZonedDateTime, ZoneId}

/**
 * Demonstrates how to build and merge schedules chain.
 *
 * This example:
 *   - Validates and creates a base oncall schedule chain with no conflicts and no gaps
 *   - Validates and creates an override chain with no conflicts
 *   - Merge both to create a new oncall schedule.
 *
 * The commented out code below demonstrates how phantom types in action to prevent illegal merging operations.
 */
object PlayScheduleDemo:
  @main def run(): Unit =
    val oncall = ScheduleChain
      .makeContinuous(
        Schedules.make("Isa", "2025-10-10T08:00:00Z".instant, "2025-10-10T16:00:00Z".instant),
        Schedules.make("Lipe", "2025-10-10T16:00:00Z".instant, "2025-10-11T16:00:00Z".instant),
        Schedules.make("Marcelle", "2025-10-11T16:00:00Z".instant, "2025-10-12T10:00:00Z".instant)
      )
      .toOption
      .get

    val overrides = ScheduleChain
      .makeAllowingGaps(
        Schedules.make("Rafa", "2025-10-10T08:00:00Z".instant, "2025-10-10T16:00:00Z".instant),
        Schedules.make("Si", "2025-10-11T15:00:00Z".instant, "2025-10-11T17:00:00Z".instant)
      )
      .toOption
      .get

    ScheduleChain.mergeStrict(oncall, overrides) match
      case Left(errors) =>
        println(s"Merging error: $errors")

      case Right(merged) =>
        println("===Base===")
        oncall.schedules.foreach(println)
        println()
        println("===Overrides===")
        overrides.schedules.foreach(println)
        println()
        println("===Merged===")
        merged.schedules.foreach(println)

    /*
     * Won't compile: can't merge non-continuous base schedules.
     */

    // val gapped = ScheduleChain
    //   .makeAllowingGaps(Seq(Schedules.make("Isa", "2025-10-10T08:00:00Z".instant, "2025-10-10T16:00:00Z".instant)))
    //   .toOption
    //   .get
    // val invalidMerge = ScheduleChain.merge(gapped, overrides)

  extension (s: String)
    def instant: Instant = Instant.parse(s)
    def zoned(zoneId: ZoneId): ZonedDateTime = ZonedDateTime.parse(s).withZoneSameInstant(zoneId)
    def local: LocalDateTime = LocalDateTime.parse(s)
