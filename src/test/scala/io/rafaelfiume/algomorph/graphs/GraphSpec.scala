package io.rafaelfiume.algomorph.graphs

import munit.FunSuite
import io.rafaelfiume.algomorph.graphs.Graph.Edge

class GraphSpec extends FunSuite with GraphsContext:

  test("vertices returns all nodes in insertion order"):
    assertEquals(g2WFourEdgeTypes.vertices, List(a, b, e, d, c, f))

  test("builds weighted graphs"):
    val e = Edge.directed(a, b, -5)

    val result = Graph.empty.add(e)

    assertEquals(result.vertices, List(a, b)) // Check weight
