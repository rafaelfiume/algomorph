package fp.ds.graphs

import fp.ds.graphs.DepthFirstSearch.traverse
import munit.Assertions.*
import munit.FunSuite

class DepthFirstSearchSpec extends FunSuite:

  test("traverse a graph visiting nodes children first") {
    val graph = List(("m", "n"), ("m", "o"), ("m", "p"), ("n", "q"), ("o", "r"), ("p", "q"), ("q", "r"), ("q", "s"))

    val result = traverse("m", graph)

    assertEquals(result, List("m", "n", "q", "r", "s", "o", "p"))
  }
