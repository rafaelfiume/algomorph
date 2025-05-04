package graphs

import munit.FunSuite
import graphs.Graph.Vertex
import graphs.Graph.Edge
import graphs.traversal.*

class GraphSpec extends FunSuite with GraphContext:

  test("Dfs traverses a graph"): // topological sort
    assertEquals(Dfs.traverse(g1).visited, List(a, b, f, j, c, g, h, d))
    assertEquals(Dfs.traverse(g1, c).visited, List(c, g, h))
    assertEquals(Dfs.traverse(g2).visited, List(a, b, e, d, c, f))

  test("Dfs (Fp) traverses a graph"):
    assertEquals(Dfs.Fp.traverse(g1).visited, List(a, b, f, j, c, g, h, d))
    assertEquals(Dfs.traverse(g1, c).visited, List(c, g, h))
    assertEquals(Dfs.Fp.traverse(g2).visited, List(a, b, e, d, c, f))

  test("Dfs finds a path between vertices"):
    val result = Dfs.traverse(g1)
    assertEquals(result.path(a, d), List(a, d))
    assertEquals(result.path(a, j), List(a, b, f, j))
    assertEquals(result.path(b, h), Nil) // no path between b and h
    assertEquals(result.path(z, d), Nil) // z is not in the graph
    assertEquals(result.path(a, z), Nil) // z is not in the graph

  test("Dfs (Fp) finds a path between vertices"):
    val result = Dfs.Fp.traverse(g1)
    println(s"${Dfs.traverse(g1).parents}")
    println(s"${result.parents}")
    assertEquals(result.path(a, d), List(a, d))
    assertEquals(result.path(a, j), List(a, b, f, j))
    assertEquals(result.path(b, h), Nil) // no path between b and h
    assertEquals(result.path(z, d), Nil) // z is not in the graph
    assertEquals(result.path(a, z), Nil) // z is not in the graph

  test("Dfs classifies edges"):
    val result = Dfs.traverse(g2)
    println(s"${result.treeEdges()}")
    assertEquals(result.treeEdges(), expectedG2TreeEdges)
    assertEquals(result.forwardEdges(), expectedG2ForwardEdges)
    assertEquals(result.backEdges(), expectedG2BackEdges)
    assertEquals(result.crossEdges(), expectedG2CrossEdges)

  test("Dfs (Fp) classifies edges"):
    val result = Dfs.Fp.traverse(g2)
    assertEquals(result.treeEdges(), expectedG2TreeEdges)
    assertEquals(result.backEdges(), expectedG2BackEdges)
    assertEquals(result.forwardEdges(), expectedG2ForwardEdges)
    assertEquals(result.crossEdges(), expectedG2CrossEdges)

  test("Bfs traverses a graph"):
    assertEquals(Bfs.traverse(g1).visited, List(a, b, c, d, f, g, j, h))
    assertEquals(Bfs.traverse(g2).visited, List(a, b, d, e, c, f))

  test("Bfs finds the shortest path between vertices in an unweighted graph"):
    val result = Bfs.traverse(g1)
    assertEquals(result.path(a, j), List(a, d, j))
    assertEquals(result.path(b, h), Nil) // no path between b and h
    assertEquals(result.path(z, d), Nil) // z is not in the graph
    assertEquals(result.path(a, z), Nil) // z is not in the graph

  test("Bfs (FP) traverses a graph"):
    assertEquals(Bfs.Fp.traverse(g1).visited, List(a, b, c, d, f, g, j, h))
    assertEquals(Bfs.Fp.traverse(g2).visited, List(a, b, d, e, c, f))
    assertEquals(Bfs.Fp.traverse(g1).path(a, j), List(a, d, j))

  test("build a graph"):
    assertEquals(g2.vertices, List(a, b, e, d, c, f))

trait GraphContext:
  val a = Vertex("A")
  val b = Vertex("B")
  val c = Vertex("C")
  val d = Vertex("D")
  val e = Vertex("E")
  val f = Vertex("F")
  val g = Vertex("G")
  val h = Vertex("H")
  val i = Vertex("I")
  val j = Vertex("J")
  val z = Vertex("Z")

  val g1 = Graph.make(
    Edge.directed(a, b),
    Edge.directed(a, c),
    Edge.directed(a, d),
    Edge.directed(b, f),
    Edge.directed(c, g),
    Edge.directed(d, f),
    Edge.directed(d, j),
    Edge.directed(f, j),
    Edge.directed(g, h)
  )

  val g2 = Graph.make(
    Edge.directed(a, b),
    Edge.directed(b, e),
    Edge.directed(e, d),
    Edge.directed(a, d), // forward
    Edge.directed(d, b), // back
    Edge.directed(c, e), // cross
    Edge.directed(c, f),
    Edge.directed(f, f) // back
  )
  val expectedG2TreeEdges = Set(Edge.directed(a, b), Edge.directed(b, e), Edge.directed(e, d), Edge.directed(c, f))
  val expectedG2ForwardEdges = Set(Edge.directed(a, d))
  val expectedG2BackEdges = Set(Edge.directed(d, b), Edge.directed(f, f))
  val expectedG2CrossEdges = Set(Edge.directed(c, e))
