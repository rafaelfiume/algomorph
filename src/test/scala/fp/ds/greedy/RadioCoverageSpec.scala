package fp.ds.greedy

import munit.Assertions.*
import munit.FunSuite

class RadioCoverageSpec extends FunSuite:

  test("bestCoverage returns the minimum number of radio stations that covers all the required states") {
    val statesNeeded = Set("mt", "wa", "or", "id", "nv", "ut", "ca", "az")
    val stations = Map(
      "kone" -> Set("id", "nv", "ut"),
      "ktwo" -> Set("wa", "id", "mt"),
      "kthree" -> Set("or", "nv", "ca"),
      "kfour" -> Set("nv", "ut"),
      "kfive" -> Set("ca", "az")
    )

    val result = RadioCoverage.best(stations)(statesNeeded)

    assertEquals(result, Set("kthree", "ktwo", "kfour", "kfive"))
    assertEquals(
      stations.filter { case (k, _) => result.contains(k) }.values.toSet.flatten,
      statesNeeded
    )
  }
