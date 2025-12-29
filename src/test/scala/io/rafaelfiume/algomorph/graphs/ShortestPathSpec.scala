package io.rafaelfiume.algomorph.graphs

import munit.FunSuite
import io.rafaelfiume.algomorph.graphs.Graph.Edge
import io.rafaelfiume.algomorph.testkit.syntax.EitherSyntax.*

class ShortestPathSpec extends FunSuite with GraphsContext:

  test("shortest path between connected vertices in a DAG"):
    val result = ShortestPath.Dag.find(graph, a, g).rightOrFail
    assertEquals(result.path(a, g), expected = List(a, c, d, e, g))
    assertEquals(result.distance, 9L)

  test("no path between disconnected vertices in a DAG"):
    val result = ShortestPath.Dag.find(graph, h, g).rightOrFail
    assertEquals(result.path(h, g), expected = List.empty)
    assertEquals(result.distance, ShortestPath.Infinity)

  test("cycle detection is scoped to what is reachable from start"):
    val result = ShortestPath.Dag.find(g2WFourEdgeTypes, a, f).leftOrFail
    // g2WFourEdgeTypes has two back edges (cycles), but only d -> b is reachable from a.
    assertEquals(result, expected = Set(Edge.directed(d, b)))

  val graph = Graph.makeWeighted(
    Edge.directed(a, b, 2),
    Edge.directed(a, c, 4),
    Edge.directed(b, d, 6),
    Edge.directed(c, d, 1),
    Edge.directed(d, e, 1),
    Edge.directed(d, f, 4),
    Edge.directed(e, g, 3),
    Edge.directed(f, g, 2),
    Edge.directed(c, h, 7)
  )
