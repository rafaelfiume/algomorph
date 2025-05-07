package graphs

import munit.FunSuite
import graphs.Graph.Vertex
import graphs.Graph.Edge
import graphs.Traversals.*

class TraversalsSpec extends FunSuite with GraphsContext:

  val dfsVariants = List(
    ("Imperative Dfs", graph => Dfs.traverse(graph)),
    ("Functional Dfs", graph => Dfs.Fp.traverse(graph))
  )

  val bfsVariants = List(
    ("Imperative Bfs", graph => Bfs.traverse(graph)),
    ("Functional Bfs", graph => Bfs.Fp.traverse(graph))
  )

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
