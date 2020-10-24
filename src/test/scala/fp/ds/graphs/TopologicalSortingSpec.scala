package fp.ds.graphs

import fp.ds.graphs.TopologicalSorting._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TopologicalSortingSpec extends AnyFlatSpec with Matchers {

  "topological sorting" should "give us the correct order of performing a task" in {
    val tasks = List(
      "getup"           -> "shower",
      "shower"          -> "breakfast",
      "breakfast"       -> "dress",
      "dress"           -> "office",
      "office"          -> "dinner",

      "breakfast"       -> "leisurely_lunch",
      "leisurely_lunch" -> "movie",
      "movie"           -> "dinner",

      "dinner"          -> "bed"
    )

    val result = topsort(tasks)

    result shouldBe List("getup", "shower", "breakfast", "leisurely_lunch", "movie", "dress", "office", "dinner", "bed")
  }

  it should "detect cycle" in {
    val tasks = List(
      "getup"           -> "shower",
      "shower"          -> "breakfast",
      "breakfast"       -> "dress",
      "dress"           -> "office",
      "office"          -> "dinner",

      "breakfast"       -> "leisurely_lunch",
      "leisurely_lunch" -> "movie",
      "movie"           -> "dinner",
      "dinner"          -> "movie", // cycle

      "dinner"          -> "bed"
    )

    val result = intercept[IllegalArgumentException] {
      topsort(tasks)
    }

    result.getMessage shouldBe "detected cycle at dinner"
  }

}

/*
 * Cycle detection - no cycles:
 *
 * current path is List(office, dress, breakfast, shower, getup)
 * next is dinner
 *
 * current path is List(movie, leisurely_lunch, breakfast, shower, getup)
 * next is dinner
 *
 *
 *
 * Detecting cycles:
 *
 * current path is List(office, dress, breakfast, shower, getup)
 * next is dinner
 *
 * current path is List(dinner, office, dress, breakfast, shower, getup)
 * next is movie
 *
 * current path is List(movie, dinner, office, dress, breakfast, shower, getup)
 * next is dinner
 */
