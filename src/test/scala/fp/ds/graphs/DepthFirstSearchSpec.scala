package fp.ds.graphs

import fp.ds.graphs.DepthFirstSearch.traverse
import org.scalatest.{ FlatSpec, Matchers }

class DepthFirstSearchSpec extends FlatSpec with Matchers {

  "dsf" should "traverse a graph visiting nodes children first" in {
    val graph = List(("m", "n"), ("m", "o"), ("m", "p"),
                     ("n", "q"), ("o", "r"), ("p", "q"),
                     ("q", "r"), ("q", "s"))

    val result = traverse("m", graph)

    result shouldBe List("m", "n", "q", "r", "s", "o", "p")
  }
}
