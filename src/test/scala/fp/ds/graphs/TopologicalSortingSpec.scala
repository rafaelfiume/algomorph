package fp.ds.graphs

import fp.ds.graphs.TopologicalSorting.*
import munit.Assertions.*
import munit.FunSuite

class TopologicalSortingSpec extends FunSuite:

  test("topological sorting returns the correct order of performing a task") {
    val tasks = List(
      "getup" -> "shower",
      "shower" -> "breakfast",
      "breakfast" -> "dress",
      "dress" -> "office",
      "office" -> "dinner",
      "breakfast" -> "leisurely_lunch",
      "leisurely_lunch" -> "movie",
      "movie" -> "dinner",
      "dinner" -> "bed"
    )

    val result = topsort(tasks)

    assertEquals(
      result,
      List("getup", "shower", "breakfast", "leisurely_lunch", "movie", "dress", "office", "dinner", "bed")
    )
  }

  test("detect cycle") {
    val tasks = List(
      "getup" -> "shower",
      "shower" -> "breakfast",
      "breakfast" -> "dress",
      "dress" -> "office",
      "office" -> "dinner",
      "breakfast" -> "leisurely_lunch",
      "leisurely_lunch" -> "movie",
      "movie" -> "dinner",
      "dinner" -> "movie", // cycle
      "dinner" -> "bed"
    )
    interceptMessage[IllegalArgumentException]("detected cycle at dinner") {
      topsort(tasks)
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
