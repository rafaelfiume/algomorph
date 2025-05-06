package graphs

import munit.FunSuite
import graphs.Graph.Vertex
import graphs.Graph.Edge
import graphs.traversal.*

class GraphSpec extends FunSuite with GraphContext:

  val dfsVariants = List(
    ("Imperative Dfs", graph => Dfs.traverse(graph)),
    ("Functional Dfs", graph => Dfs.Fp.traverse(graph))
  )

  val bfsVariants = List(
    ("Imperative Bfs", graph => Bfs.traverse(graph)),
    ("Functional Bfs", graph => Bfs.Fp.traverse(graph))
  )

  /**
   * Traversals
   */

  dfsVariants.foreach { (description, dfs) =>
    test(s"$description traverses a graph from the default starting node"):
      assertEquals(dfs(g1).visited, List(a, b, f, j, c, g, h, d))
      assertEquals(dfs(g2WFourEdgeTypes).visited, List(a, b, e, d, c, f))
  }

  dfsVariants.foreach { (description, dfs) =>
    test(s"$description traverses a graph from a provided starting node"):
      assertEquals(dfs(g2WFourEdgeTypes).visited, List(a, b, e, d, c, f))
  }

  dfsVariants.foreach { (description, dfs) =>
    test(s"$description finds a path between vertices"):
      val result = dfs(g1)
      assertEquals(result.path(a, d), List(a, d))
      assertEquals(result.path(a, j), List(a, b, f, j))
      assertEquals(result.path(b, h), Nil) // no path between b and h
      assertEquals(result.path(z, d), Nil) // z is not in the graph
      assertEquals(result.path(a, z), Nil) // z is not in the graph
  }

  dfsVariants.foreach { (description, dfs) =>
    test(s"$description classifies edges"):
      val result = dfs(g2WFourEdgeTypes)
      assertEquals(result.treeEdges(), g2TreeEdges)
      assertEquals(result.backEdges(), g2BackEdges)
      assertEquals(result.forwardEdges(), g2ForwardEdges)
      assertEquals(result.crossEdges(), g2CrossEdges)
  }

  bfsVariants.foreach { (description, bfs) =>
    test(s"$description traverses a graph"):
      assertEquals(bfs(g1).visited, List(a, b, c, d, f, g, j, h))
      assertEquals(bfs(g2WFourEdgeTypes).visited, List(a, b, d, e, c, f))
  }

  // TODO Test it from a explicit starting point

  bfsVariants.foreach { (description, bfs) =>
    test(s"$description finds a path between vertices"):
      assertEquals(bfs(g1).path(a, j), List(a, d, j))
  }

  /**
   * Topological order
   */

  dfsVariants.foreach { (description, dfs) =>
    test(s"$description topologically sorts a graph"):
      assertEquals(dfs(multipleDags).sort(), Right(List(i, d, e, a, b, c, f, g, h)))
  }

  dfsVariants.foreach { (description, dfs) =>
    test(s"$description topological sort handles cycles in a graph"):
      assertEquals(dfs(g2WFourEdgeTypes).sort(), Left(g2BackEdges))
  }

  /**
   * Shortest path (Unweighted graphs)
   */

  bfsVariants.foreach { (description, bfs) =>
    test(s"$description finds the shortest path between vertices in an unweighted graph"):
      val result = bfs(g1)
      assertEquals(result.path(a, j), List(a, d, j))
      assertEquals(result.path(b, h), Nil) // no path between b and h
      assertEquals(result.path(z, d), Nil) // z is not in the graph
      assertEquals(result.path(a, z), Nil) // z is not in the graph
  }

  /**
   * Graphs
   */

  test("vertices returns all nodes in insertion order"):
    assertEquals(g2WFourEdgeTypes.vertices, List(a, b, e, d, c, f))

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

  // See: https://ocw.mit.edu/courses/6-006-introduction-to-algorithms-fall-2011/e59f8a55929028498953691891229a17_MIT6_006F11_lec14.pdf
  val g2WFourEdgeTypes = Graph.make(
    Edge.directed(a, b),
    Edge.directed(b, e),
    Edge.directed(e, d),
    Edge.directed(a, d), // forward
    Edge.directed(d, b), // back
    Edge.directed(c, e), // cross
    Edge.directed(c, f),
    Edge.directed(f, f) // back
  )
  val g2TreeEdges = Set(Edge.directed(a, b), Edge.directed(b, e), Edge.directed(e, d), Edge.directed(c, f))
  val g2ForwardEdges = Set(Edge.directed(a, d))
  val g2BackEdges = Set(Edge.directed(d, b), Edge.directed(f, f))

  val g2CrossEdges = Set(Edge.directed(c, e))
  // Dag: Directed Acyclic Graph
  // See: https://ocw.mit.edu/courses/6-006-introduction-to-algorithms-fall-2011/e59f8a55929028498953691891229a17_MIT6_006F11_lec14.pdf
  val multipleDags = Graph
    .make(
      Edge.directed(g, h), // g -> h (g is a prerequisite for h - g must come before h)
      Edge.directed(a, h),
      Edge.directed(a, b),
      Edge.directed(b, c),
      Edge.directed(c, f),
      Edge.directed(d, c),
      Edge.directed(d, e),
      Edge.directed(e, f)
    )
    .add(i)
