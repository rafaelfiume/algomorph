package io.rafaelfiume.algomorph.graphs

import io.rafaelfiume.algomorph.graphs.Graph.Vertex
import scala.collection.mutable
import io.rafaelfiume.algomorph.graphs.Traversals.Dfs
import io.rafaelfiume.algomorph.graphs.Graph.Edge

case class ShortestPath[V <: Vertex](
  val parents: Map[V, V],
  val distance: Long
):
  def path(start: V, end: V): List[V] = Path.reconstruct(parents, start, end)

object ShortestPath:

  object Dag:
    /*
     * Finds the shortest path in non-negative weighted DAGs.
     *
     * ===Math===
     * Let G(V, E) be a directed graph with a weight function w: E -> Z.
     * w(π), the weight of a path π, is the sum of weights of edges in the path.
     *
     * The shortest path distance from vertex s to t is defined as:
     *   δ(s, t) = inf{w(π) | π is a path from s to t}
     *
     * - If there are no paths between s and t, then δ(s, t) = ∞
     * - Triangle inequality holds: δ(u, v) <= δ(u, x) + δ (x, v)
     * - At the end of the algorithm, the computed distance d(u, v) is equal to δ(u, v).
     *
     * Negative-weight cycles is a path π that starts and ends at the same vertex `s` with w(π) < 0.
     *   - In such cases, δ(s, t) = -∞
     *   - Thus, `inf` instead of `min` since a finite-length minimum might not exists due to negative-weight cycles
     *   - This algorithm assumes a DAG, so negative-weight cycles cannot occur.
     *
     * ===Algorithm===
     * The key idea is to leverage the acyclic structure of the graph.
     *
     * The topological order of a DAG ensures that for any edge (u, v) where top_order(u) < top_order(v),
     * u is processed before v, guaranteeing that all optimal subpaths are processed before their dependants.
     *
     * In this way, each vertex is relaxed only once, allowing linear time complexity.
     *
     *===Complexity===
     *  - Time: Θ(V + E)
     */
    def find[V <: Vertex](graph: Graph[V], start: V, end: V): Either[Set[Edge[V]], ShortestPath[V]] =
      val parents: mutable.HashMap[V, V] = mutable.HashMap.empty
      val dist: mutable.Map[V, Long] = mutable.HashMap.empty.withDefaultValue(Infinity)
      Dfs.traverse(graph, start).topologicalSort() match
        case Left(backEdges) => Left(backEdges)

        case Right(topOrder) =>
          dist(start) = 0
          for u <- topOrder; v <- graph.adjacencies(u) do
            if dist(v) > dist(u) + graph.w(u, v) then
              dist(v) = dist(u) + graph.w(u, v)
              parents(v) = u
          Right(ShortestPath(parents.toMap, dist(end)))

  // it's odd to name a val with `Long.MaxValue` "infinity", but it makes the algorithm read nicer.
  val Infinity = Long.MaxValue
