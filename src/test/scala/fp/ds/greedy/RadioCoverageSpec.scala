package fp.ds.greedy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RadioCoverageSpec extends AnyFlatSpec with Matchers {

  "bestCoverage" should "return the minimum number of radio stations that covers all the required states" in {
    val statesNeeded = Set("mt", "wa", "or", "id", "nv", "ut", "ca", "az")
    val stations = Map(
      "kone"   -> Set("id", "nv", "ut"),
      "ktwo"   -> Set("wa", "id", "mt"),
      "kthree" -> Set("or", "nv", "ca"),
      "kfour"  -> Set("nv", "ut"),
      "kfive"  -> Set("ca", "az")
    )

    val result = RadioCoverage.best(stations)(statesNeeded)

    result should contain theSameElementsAs Set("kthree", "ktwo", "kfour", "kfive")
    stations.filter { case (k, _) => result.contains(k) }.values.toSet.flatten shouldBe statesNeeded
  }

}
