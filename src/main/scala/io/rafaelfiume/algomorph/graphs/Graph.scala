package io.rafaelfiume.algomorph.graphs

import Graph.*
import io.rafaelfiume.algomorph.graphs.Graph.Edge.Weight

/*
 * Useful links:
 * - [Review on Graphs](https://ocw.mit.edu/courses/6-006-introduction-to-algorithms-fall-2011/6d8aaabf67d391877f978172b293d7be_MIT6_006F11_rec13.pdf)
 * - [BFS for Shortest Path & Graph Transformation](https://ocw.mit.edu/courses/6-006-introduction-to-algorithms-fall-2011/resources/recitation-15-shortest-paths/)
 * - [DFS Edge Classification](https://courses.csail.mit.edu/6.006/fall11/rec/rec14.pdf)
 * - [More about Edges Classification & Graph Transformation](https://courses.csail.mit.edu/6.006/fall10/handouts/quiz2review.pdf)
 */
object Graph:

  enum Vertex:
    case Simple[T](value: T)

    override def toString(): String = this match
      case Simple(value) => s"$value"

  object Vertex:
    def apply[T](value: T) = Vertex.Simple(value)

  case class Edge[T](u: T, v: T)

  object Edge:
    type Weight = Long

    def directed[V](u: V, v: V) = Edge(u, v)
    def directed[V](u: V, v: V, w: Weight): (Edge[V], Weight) = Edge(u, v) -> w

    enum Classification:
      case TreeEdge
      case ForwardEdge
      case BackEdge
      case CrossEdge

  def empty[V <: Vertex]: Graph[V] = Graph(Map.empty, Nil, Map.empty)

  def make[V <: Vertex](edges: Edge[V]*) = edges.foldLeft(Graph.empty[V]) { _.add(_) }
  def makeWeighted[V <: Vertex](edges: (Edge[V], Weight)*) = edges.foldLeft(Graph.empty[V]) { _.add(_) }

/*
 * Implements an explicit representation of graph G = (V, E).
 *
 * Space complexity is O(V + E).
 */
case class Graph[V <: Vertex] private (
  private val adjacencyList: Map[V, List[V]],
  val vertices: List[V],
  private val weights: Map[Edge[V], Weight] // allows querying the weight of an edge in O(1)
):
  /**
   * ===Complexity===
   * Per-operation time complexity is O(V) dominated by`++` and `distinct`, both O(n). Time complexity for adding E edges is
   * O(E*V), which might become O(V^2) or even O(V^3).
   */
  def add(edge: Edge[V]): Graph[V] =
    val updatedAdjList = adjacencyList.updatedWith(edge.u)(_.map(_ :+ edge.v).orElse(Some(List(edge.v))))
    val updatedVs = (vertices ++ List(edge.u, edge.v)).distinct
    Graph(updatedAdjList, updatedVs, weights)

  def add(edge: (Edge[V], Weight)): Graph[V] =
    val e = edge._1
    val weight = edge._2
    add(e).copy(weights = weights + (e -> weight))

  // Careful with this function, yolo mode for now.
  // In particular, it will override all the edges of `vertex` that might have been added with `add(Edge)`.
  /*
   * ===Complexity===
   * Per-operation time complexity is O(V) due to O(n) required to append vertices in a list on each `add` call.
   * Time complexity for adding V vertices is O(V^2).
   */
  def add(vertex: V): Graph[V] =
    Graph(adjacencyList + (vertex -> Nil), vertices :+ vertex, weights)

  def contains(v: V): Boolean = adjacencyList.contains(v)

  def w(u: V, v: V): Long = weights(Edge.directed(u, v))

  protected[graphs] def adjacencies(v: V): List[V] = adjacencyList.get(v).fold(Nil)(identity)

// TODO Build graph with either only directed edges or only undirected edges
// TODO Implicit representation of a graph when it's not possible or convenient to store the entire graph in memory,
// e.g. when solving the Rubik's cube problem: adj computes the vertices.
// TODO Optimise `add` ops, perhaps by replacing Map[V, List[V]] by Map[V, Vector[V]] and List[V] by LinkedHashSet[V]?
