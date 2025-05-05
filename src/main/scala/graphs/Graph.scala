package graphs

import Graph.*
import scala.collection.mutable
import scala.collection.immutable
import graphs.Graph.Edge.Classification
import graphs.Graph.Edge.Classification.*
import graphs.traversal.TraversalResult.WithEdges
import graphs.traversal.TraversalResult.Basic
import scala.collection.immutable.Queue

/*
 * Useful links:
 * - [Review on Graphs](https://ocw.mit.edu/courses/6-006-introduction-to-algorithms-fall-2011/6d8aaabf67d391877f978172b293d7be_MIT6_006F11_rec13.pdf)
 * - [BFS for Shortest Path & Graph Transformation](https://ocw.mit.edu/courses/6-006-introduction-to-algorithms-fall-2011/resources/recitation-15-shortest-paths/)
 * - [Depth-First Search](https://ocw.mit.edu/courses/6-006-introduction-to-algorithms-fall-2011/e59f8a55929028498953691891229a17_MIT6_006F11_lec14.pdf)
 * - [DFS Edge Classification](https://courses.csail.mit.edu/6.006/fall11/rec/rec14.pdf)
 * - [More about Edges Classification & Graph Transformation](https://courses.csail.mit.edu/6.006/fall10/handouts/quiz2review.pdf)
 */

object Graph:

  enum Vertex:
    case Simple[T](value: T)

    override def toString(): String = this match
      case Simple(value) => s"V($value)"

  object Vertex:
    def apply[T](value: T) = Vertex.Simple(value)

  case class Edge[T](u: T, v: T)

  object Edge:
    def directed[T](u: T, v: T) = Edge(u, v)

    enum Classification:
      case TreeEdge
      case ForwardEdge
      case BackEdge
      case CrossEdge

  def empty[V <: Vertex](): Graph[V] = Graph(Map.empty, Nil)

  def make[V <: Vertex](edges: Edge[V]*) = edges.foldLeft(Graph.empty[V]()) { _.add(_) }

// TODO Build graph with either only directed edges or only undirected edges
// TODO Implicit representation of a graph when it's not possible or convenient to store the entire graph in memory,
// e.g. when solving the Rubik's cube problem: adj computes the vertices.
/*
 * Implements an explicit representation of graph G = (V, E).
 *
 * Space complexity is O(V + E).
 */
class Graph[V <: Vertex] private (
  private val adjacencyList: Map[V, List[V]],
  val vertices: List[V]
):
  def add(edge: Edge[V]): Graph[V] =
    val updatedAdjList = adjacencyList.updatedWith(edge.u) {
      case Some(adj) => Some(adj :+ edge.v)
      case None      => Some(List(edge.v))
    }
    val updatedVs = (vertices ++ List(edge.u, edge.v)).distinct
    Graph(updatedAdjList, updatedVs)

  def contains(v: V): Boolean = adjacencyList.contains(v)

  protected[graphs] def adjacent(v: V): List[V] = adjacencyList.get(v).fold(Nil)(identity)

/*
 * Requirements:
 *  - Deterministic Order: consistent traversal across runs
 *  - Complete coverage: visit all vertices
 *  - Component awareness: handle disconected subgraphs.
 */
object traversal:

  sealed trait TraversalResult[V <: Vertex]:
    def visited: List[V]
    def parents: Map[V, V]

    def path(start: V, end: V): List[V] =
      @annotation.tailrec
      def loop(end: V, acc: List[V]): List[V] =
        if start == end then start :: acc
        else
          parents.get(end) match
            case None         => Nil
            case Some(parent) => loop(parent, end :: acc)

      loop(end, Nil)

  object TraversalResult:
    case class Basic[V <: Vertex](visited: List[V], parents: Map[V, V]) extends TraversalResult[V]

    case class WithEdges[V <: Vertex](
      visited: List[V],
      parents: Map[V, V],
      edgesClassification: Map[Edge[V], Edge.Classification]
    ) extends TraversalResult[V]:
      def treeEdges(): Set[Edge[V]] = filterEdges(byType = TreeEdge)
      def forwardEdges(): Set[Edge[V]] = filterEdges(byType = ForwardEdge)
      def backEdges(): Set[Edge[V]] = filterEdges(byType = BackEdge)
      def crossEdges(): Set[Edge[V]] = filterEdges(byType = CrossEdge)
      private def filterEdges(byType: Classification): Set[Edge[V]] =
        edgesClassification.filter { (_, edgeType) => edgeType == byType }.keySet.toSet

  /*
   * Particularly useful for edge classification (e.g detecting cyles, topoligical sorting).
   */
  object Dfs:

    case class EdgeClassifier[V <: Vertex]():
      private val edges = mutable.HashMap.empty[Edge[V], Edge.Classification]
      private val discovery = mutable.HashMap.empty[V, Long]
      private val finish = mutable.HashMap.empty[V, Long]

      def addEdge(u: V, v: V, firstTimeVisited: V => Boolean): Unit =
        val edge = Edge.directed(u, v)
        if firstTimeVisited(v) then edges += edge -> TreeEdge
        else if discoveryTime(v) <= discoveryTime(u) && !hasFinishedBeingVisited(v) then edges += edge -> BackEdge
        else if discoveryTime(v) > discoveryTime(u) then edges += edge -> ForwardEdge
        else if discoveryTime(v) < discoveryTime(u) then edges += edge -> CrossEdge

      def setDiscoveryTime(vertex: V, time: Long) = discovery(vertex) = time
      def discoveryTime(vertex: V) = discovery.getOrElse(vertex, Long.MinValue)

      def setFinishTime(vertex: V, time: Long) = finish(vertex) = time
      def finishTime(vertex: V) = discovery.getOrElse(vertex, Long.MaxValue)

      def classification(): Map[Edge[V], Edge.Classification] = edges.toMap

      private def hasFinishedBeingVisited(vertex: V) = finish.contains(vertex)

    def traverse[V <: Vertex](graph: Graph[V]): TraversalResult.WithEdges[V] =
      val state = TraversalState.empty[V]()
      for v <- graph.vertices do if state.firstTimeVisited(v) then dfs(state)(graph, v, state.parents.get(v))
      WithEdges(state.visited.toList, state.parents.toMap, state.edges.classification())

    def traverse[V <: Vertex](graph: Graph[V], start: V): TraversalResult.WithEdges[V] =
      val state = TraversalState.empty[V]()
      dfs(state)(graph, start)
      WithEdges(state.visited.toList, state.parents.toMap, state.edges.classification())

    private def dfs[V <: Vertex](state: TraversalState[V])(graph: Graph[V], start: V, parent: Option[V] = None): Unit =
      def visit(vertex: V, parent: Option[V] = None): Unit =
        state.discovered(vertex)
        state.addVisited(vertex)
        parent.foreach(p =>
          state.addParent(p, vertex)
          state.addEdge(p, vertex, firstTimeVisited = true)
        )
        graph.adjacent(vertex).foreach { neighbour =>
          if state.firstTimeVisited(neighbour) then visit(neighbour, Some(vertex))
          else state.addEdge(vertex, neighbour, firstTimeVisited = false)
        }
        state.finished(vertex)

      visit(start, parent)

    private object TraversalState:
      def empty[V <: Vertex](): TraversalState[V] =
        TraversalState[V](mutable.ArrayBuffer.empty[V], mutable.HashMap.empty[V, V], new EdgeClassifier[V]())

    private case class TraversalState[V <: Vertex](
      visited: mutable.ArrayBuffer[V],
      parents: mutable.HashMap[V, V],
      edges: EdgeClassifier[V]
    ):
      private var time = 0

      def firstTimeVisited(vertex: V): Boolean = !parents.contains(vertex)
      def addParent(parent: V, vertex: V): Unit = parents(vertex) = parent
      def addVisited(vertex: V): Unit = visited += vertex

      def discovered(vertex: V): Unit =
        time += 1
        edges.setDiscoveryTime(vertex, time)

      def finished(vertex: V): Unit =
        edges.setFinishTime(vertex, time)

      def addEdge(u: V, v: V, firstTimeVisited: Boolean): Unit = edges.addEdge(u, v, _ => firstTimeVisited)

    object Fp:
      def traverse[V <: Vertex](graph: Graph[V]): TraversalResult.WithEdges[V] =
        val state = graph.vertices.foldLeft(TraversalState.empty[V]) { (state, v) =>
          if state.firstTimeVisited(v) then dfsVisit(state)(graph, v)
          else state
        }
        WithEdges(state.visited.toList.reverse, state.parents.toMap, state.edges.classification())

      def traverse[V <: Vertex](graph: Graph[V], start: V): TraversalResult.WithEdges[V] =
        val state = dfsVisit(TraversalState.empty[V])(graph, start)
        WithEdges(state.visited.reverse, state.parents, state.edges.classification())

      private def dfsVisit[V <: Vertex](state: TraversalState[V])(graph: Graph[V], start: V): TraversalState[V] =
        @annotation.tailrec
        def loop(state: TraversalState[V]): TraversalState[V] =
          state.time_++.pop() match
            case (None, poppedState) => poppedState

            case (Some(vertex -> Some(marker)), poppedState) if vertex == marker =>
               loop { poppedState.finished(vertex) }

            case (Some(vertex -> Some(parent)), poppedState) if !poppedState.firstTimeVisited(vertex) =>
              poppedState.addEdge(parent, vertex, false)

            case (Some(vertex -> parent), poppedState) =>
              loop {
                val nextState = parent
                  .fold(ifEmpty = poppedState) { parent => poppedState.addParent(parent, vertex).addEdge(parent, vertex, true) }
                  .discovered(vertex)
                  .addVisited(vertex)
                  .push(vertex -> Some(vertex)) // marker to trigger the finish phase enabling tailrec
                graph.adjacent(vertex).foldRight(nextState) { (neighbour, state) =>
                  if state.firstTimeVisited(neighbour) then state.push(neighbour -> Some(vertex))
                  else state.addEdge(vertex, neighbour, false)
                }
              }
        loop(state.push(start -> None))

      private object TraversalState:
        def empty[V <: Vertex] =
          TraversalState[V](List.empty[(V, Option[V])], List.empty[V], Map.empty[V, V], new EdgeClassifier[V]())

      private case class TraversalState[V <: Vertex](
        stack: List[(V, Option[V])],
        visited: List[V],
        parents: Map[V, V],
        edges: EdgeClassifier[V], // TODO Mutable
        time: Long = 0
      ):
        def firstTimeVisited(vertex: V): Boolean = !parents.contains(vertex)
        def push(vertexWParent: (V, Option[V])): TraversalState[V] = copy(stack = vertexWParent :: this.stack)
        def pop(): (Option[(V, Option[V])], TraversalState[V]) = stack match
          case Nil     => None -> this
          case v :: vs => Some(v) -> copy(stack = vs)
        def addParent(parent: V, vertex: V): TraversalState[V] = copy(parents = parents + (vertex -> parent))
        def addVisited(vertex: V): TraversalState[V] = copy(visited = vertex :: this.visited)

        def time_++ : TraversalState[V] = copy(time = time + 1)

        def discovered(vertex: V): TraversalState[V] =
          edges.setDiscoveryTime(vertex, time)
          this

        def finished(vertex: V): TraversalState[V] =
          edges.setFinishTime(vertex, time)
          this

        def addEdge(u: V, v: V, firstTimeVisited: Boolean): TraversalState[V] =
          edges.addEdge(u, v, _ => firstTimeVisited)
          this

  /*
   * Best for finding the shortest-path in unweighted graphs.
   *
   * TODO (test) It should work both in directed and undirect graphs.
   */
  object Bfs:

    /*
     * Traverses all the vertices.
     *
     * Time complexity: O(V + E).
     */
    def traverse[V <: Vertex](graph: Graph[V]): TraversalResult.Basic[V] =
      val state = TraversalState.empty[V]()
      for v <- graph.vertices do if state.firstTimeVisited(v) then bfs(state)(graph, v)
      Basic(state.visited.toList, state.parents.toMap)

    /*
     * Traverses all the vertices reachable from `start`.
     *
     * Time complexity: O(E).
     */
    def traverse[V <: Vertex](graph: Graph[V], start: V): TraversalResult.Basic[V] =
      val state = TraversalState.empty[V]()
      bfs(state)(graph, start)
      Basic(state.visited.toList, state.parents.toMap)

    private def bfs[V <: Vertex](state: TraversalState[V])(graph: Graph[V], start: V): Unit =
      state.queue(start)
      while state.queueNonEmpty() do
        val vertex = state.dequeue()
        state.visited += vertex
        graph.adjacent(vertex).foreach { neighbour =>
          if state.firstTimeVisited(neighbour) then
            state.marked += neighbour
            state.addParent(vertex, neighbour)
            state.queue(neighbour)
        }

    private object TraversalState:
      def empty[V <: Vertex]() =
        TraversalState(mutable.Set.empty[V], mutable.ArrayBuffer.empty[V], mutable.HashMap.empty[V, V], mutable.Queue.empty[V])

    private case class TraversalState[V <: Vertex](
      marked: mutable.Set[V],
      visited: mutable.ArrayBuffer[V],
      parents: mutable.HashMap[V, V],
      queue: mutable.Queue[V]
    ):
      def firstTimeVisited(vertex: V): Boolean = !marked.contains(vertex)
      def queue(vertex: V): Unit = queue.addOne(vertex)
      def dequeue(): V = queue.dequeue()
      def queueNonEmpty(): Boolean = queue.nonEmpty
      def mark(vertex: V): Unit = marked += vertex
      def addParent(parent: V, vertex: V): Unit = parents(vertex) = parent
      def addVisited(vertex: V): Unit = visited += vertex

    /*
     * Pure, stack-safe and concurrent-safe functions.
     */
    object Fp:
      def traverse[V <: Vertex](graph: Graph[V]): TraversalResult.Basic[V] =
        val state = graph.vertices.foldLeft(TraversalState.empty[V]()) { (state, v) =>
          if state.firstTimeVisited(v) then bfs(state.queue(v))(graph)
          else state
        }
        Basic(state.visited.toList.reverse, state.parents.toMap)

      def traverse[V <: Vertex](graph: Graph[V], start: V): TraversalResult.Basic[V] =
        val state = bfs(TraversalState.make(start))(graph)
        Basic(state.visited.toList.reverse, state.parents.toMap)

      private def bfs[V <: Vertex](state: TraversalState[V])(graph: Graph[V]): TraversalState[V] =
        @annotation.tailrec
        def loop(state: TraversalState[V]): TraversalState[V] =
          state.queue.dequeueOption match
            case None              => state
            case Some((vertex, q)) =>
              // val currentState = state.queue(current).addVisited(current).mark(current)
              val currentState = state.copy(queue = q, visited = vertex :: state.visited, marked = state.marked + vertex)
              loop {
                graph
                  .adjacent(vertex)
                  .foldLeft(currentState) { (state, neighbour) =>
                    if state.firstTimeVisited(neighbour) then state.queue(neighbour).mark(neighbour).addParent(vertex, neighbour)
                    else state
                  }
              }
        loop(state)

      private object TraversalState:
        def empty[V <: Vertex]() =
          TraversalState(Queue.empty[V], Set.empty[V], List.empty[V], Map.empty[V, V])

        def make[V <: Vertex](v: V) =
          TraversalState(Queue(v), Set.empty[V], List.empty[V], Map.empty[V, V])

      private case class TraversalState[V <: Vertex](
        queue: Queue[V],
        marked: Set[V],
        visited: List[V],
        parents: Map[V, V]
      ):
        def firstTimeVisited(vertex: V): Boolean = !marked.contains(vertex)
        def queue(vertex: V): TraversalState[V] = copy(queue = queue.enqueue(vertex))
        def dequeue(): Option[(V, Queue[V])] = queue.dequeueOption
        def queueNonEmpty(): Boolean = queue.nonEmpty
        def mark(vertex: V): TraversalState[V] = copy(marked = this.marked + vertex)
        def addParent(parent: V, vertex: V): TraversalState[V] = copy(parents = parents + (vertex -> parent))
        def addVisited(vertex: V): TraversalState[V] = copy(visited = vertex :: this.visited)
