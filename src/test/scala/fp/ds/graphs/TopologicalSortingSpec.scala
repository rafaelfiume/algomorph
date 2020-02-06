package fp.ds.graphs

import fp.ds.graphs.TopologicalSorting.topsort
import org.scalatest.{ FlatSpec, Matchers }

class TopologicalSortingSpec extends FlatSpec with Matchers {

  "topological sorting" should "give us the correct order of performing a task" in {
    val tasks = List(
      "getup"           -> "shower",
      "shower"          -> "breakfast",
      "breakfast"       -> "dress",
      "dress"           -> "office",
      "office"          -> "dinner",

      "breakfast"       -> "leisurely_lunch",
      "leisurely_lunch" -> "movie",
      "movie"           -> "dinner"
    )

    val result = topsort(tasks)

    result shouldBe List("getup", "shower", "breakfast", "leisurely_lunch", "movie", "dress", "office", "dinner")
  }

}
