package graphs

import Graph.*
import scala.collection.mutable
import scala.collection.immutable
import graphs.Graph.Edge.Classification
import graphs.Graph.Edge.Classification.*
import graphs.Traversals.Dfs.EdgeClassifier
import graphs.Traversals.TraversalResult.WithEdges
import graphs.Traversals.TraversalResult.Basic
import scala.collection.immutable.Queue

/*
 * Requirements:
 *  - Deterministic Order: consistent traversal across runs
 *  - Complete coverage: visit all vertices
 *  - Component awareness: handle disconected subgraphs.
 */
object Traversals:

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
      private val edgeClassifier: EdgeClassifier[V]
    ) extends TraversalResult[V]:
      def treeEdges(): Set[Edge[V]] = filterEdges(byType = TreeEdge)
      def forwardEdges(): Set[Edge[V]] = filterEdges(byType = ForwardEdge)
      def backEdges(): Set[Edge[V]] = filterEdges(byType = BackEdge)
      def crossEdges(): Set[Edge[V]] = filterEdges(byType = CrossEdge)

      /*
       * Time complexity is O(E) dominated by filterEdges.
       *
       * Similarly to other functions, this one can be fairly easily optmised.
       * `hasCycle` in particular can be O(1). I may optimise and benchmark these functions at some point.
       */
      def hasCycle(): Boolean = backEdges().nonEmpty
      def hasNoCycles(): Boolean = !hasCycle()

      /*
       * Topologically sorts the vertices of a graph if there are no cycles.
       * Return the back edges, otherwise.
       *
       * A vertex `u` is before `v` if there is an edge `(u, v)` and `v` finishes before `u`.
       *
       * Time complexity is O(n log n).
       * Note: can be done in O(V + E).
       */
      def sort(): Either[Set[Edge[V]], List[V]] =
        Either.cond(hasNoCycles(), right = edgeClassifier.sort(), left = backEdges())

      private def filterEdges(byType: Classification): Set[Edge[V]] =
        // preserves space at the cost of traversing the map on each call: O(E)
        edgeClassifier.classification().filter { (_, edgeType) => edgeType == byType }.keySet.toSet

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
        // Tree: discoveryTime(u) < discoveryTime(v) < finishTime(v) < finishTime(u) if first time visited
        if firstTimeVisited(v) then edges += edge -> TreeEdge
        // Back: discoveryTime(v) < discoveryTime(u) < finishTime(u) < finishTime(v)
        else if discoveryTime(v) <= discoveryTime(u) && !hasFinishedBeingVisited(v) then edges += edge -> BackEdge
        // Forward: discoveryTime(u) < discoveryTime(v) < finishTime(v) < finishTime(u)
        else if discoveryTime(v) > discoveryTime(u) then edges += edge -> ForwardEdge
        // Cross: discoveryTime[v] < finishTime[v] < discoveryTime[u] < finishTime[u]
        else if discoveryTime(v) < discoveryTime(u) then edges += edge -> CrossEdge

      def setDiscoveryTime(vertex: V, time: Long) = discovery(vertex) = time
      def discoveryTime(vertex: V) = discovery.getOrElse(vertex, Long.MinValue)

      def setFinishTime(vertex: V, time: Long) = finish(vertex) = time
      def finishTime(vertex: V) = finish.getOrElse(vertex, Long.MaxValue)

      def classification(): Map[Edge[V], Edge.Classification] = edges.toMap

      /*
       * Time complexity is O(n log(n)) dominated by java.util.Arrays.sort.
       * Note: can be done in O(V + E).
       */
      def sort(): List[V] = finish.toSeq.sortBy(_._2).reverse.toList.map(_._1)

      private def hasFinishedBeingVisited(vertex: V) = finish.contains(vertex)

    /*
     * Connected components.
     *
     * Time complexity: O(V + E)
     */
    def traverse[V <: Vertex](graph: Graph[V]): TraversalResult.WithEdges[V] =
      val state = TraversalState.empty[V]()
      for v <- graph.vertices do if state.firstTimeVisited(v) then dfs(state)(graph, v, state.parents.get(v))
      WithEdges(state.visited.toList, state.parents.toMap, state.edgesClassification)

    /*
     * Single source reachability.
     *
     * Time complexity: O(E).
     */
    def traverse[V <: Vertex](graph: Graph[V], start: V): TraversalResult.WithEdges[V] =
      val state = TraversalState.empty[V]()
      dfs(state)(graph, start)
      WithEdges(state.visited.toList, state.parents.toMap, state.edgesClassification)

    /* Time complexity: O(E) */
    private def dfs[V <: Vertex](state: TraversalState[V])(graph: Graph[V], start: V, parent: Option[V] = None): Unit =
      def visit(vertex: V, parent: Option[V] = None): Unit =
        state.discovered(vertex)
        state.addVisited(vertex)
        parent.foreach(p =>
          state.addParent(p, vertex)
          state.addEdge(p, vertex, firstTimeVisited = true)
        )
        graph.adjacencies(vertex).foreach { neighbour =>
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
      edgesClassification: EdgeClassifier[V]
    ):
      private var discoveryTime = 0
      private var finishTime = 0

      def firstTimeVisited(vertex: V): Boolean = !parents.contains(vertex)
      def addParent(parent: V, vertex: V): Unit = parents(vertex) = parent
      def addVisited(vertex: V): Unit = visited += vertex

      def discovered(vertex: V): Unit =
        discoveryTime += 1
        // println(s"time $discoveryTime: discovered $vertex")
        edgesClassification.setDiscoveryTime(vertex, discoveryTime)

      def finished(vertex: V): Unit =
        finishTime += 1
        // println(s"time: $finishTime; finished $vertex")
        edgesClassification.setFinishTime(vertex, finishTime)

      def addEdge(u: V, v: V, firstTimeVisited: Boolean): Unit = edgesClassification.addEdge(u, v, _ => firstTimeVisited)

    object Fp:
      def traverse[V <: Vertex](graph: Graph[V]): TraversalResult.WithEdges[V] =
        val state = graph.vertices.foldLeft(TraversalState.empty[V]) { (state, v) =>
          if state.firstTimeVisited(v) then dfsVisit(state)(graph, v)
          else state
        }
        WithEdges(state.visited.toList.reverse, state.parents.toMap, state.edgesClassification)

      def traverse[V <: Vertex](graph: Graph[V], start: V): TraversalResult.WithEdges[V] =
        val state = dfsVisit(TraversalState.empty[V])(graph, start)
        WithEdges(state.visited.reverse, state.parents, state.edgesClassification)

      private def dfsVisit[V <: Vertex](state: TraversalState[V])(graph: Graph[V], start: V): TraversalState[V] =
        @annotation.tailrec
        def loop(state: TraversalState[V]): TraversalState[V] =
          state.pop() match
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
                graph.adjacencies(vertex).foldRight(nextState) { (neighbour, state) =>
                  if state.firstTimeVisited(neighbour) then state.push(neighbour -> Some(vertex))
                  else state.addEdge(vertex, neighbour, false)
                }
              }
        loop(state.push(start -> None))

      private object TraversalState:
        def empty[V <: Vertex] =
          TraversalState[V](List.empty[(V, Option[V])], List.empty[V], Map.empty[V, V], new EdgeClassifier[V]())

      private case class TraversalState[V <: Vertex](
        private val stack: List[(V, Option[V])],
        visited: List[V],
        parents: Map[V, V],
        edgesClassification: EdgeClassifier[V], // TODO This impl. of EdgeClassifier is mutable
        private val discoveryTime: Long = 0,
        private val finishTime: Long = 0
      ):
        def firstTimeVisited(vertex: V): Boolean = !parents.contains(vertex)
        def push(vertexWParent: (V, Option[V])): TraversalState[V] = copy(stack = vertexWParent :: this.stack)
        def pop(): (Option[(V, Option[V])], TraversalState[V]) = stack match
          case Nil     => None -> this
          case v :: vs => Some(v) -> copy(stack = vs)
        def addParent(parent: V, vertex: V): TraversalState[V] = copy(parents = parents + (vertex -> parent))
        def addVisited(vertex: V): TraversalState[V] = copy(visited = vertex :: this.visited)

        def discovered(vertex: V): TraversalState[V] =
          val updatedDiscoveryTime = discoveryTime + 1
          // println(s"time $updatedDiscoveryTime: discovered $vertex")
          edgesClassification.setDiscoveryTime(vertex, updatedDiscoveryTime)
          this.copy(discoveryTime = updatedDiscoveryTime)

        def finished(vertex: V): TraversalState[V] =
          val updatedFinishTime = finishTime + 1
          // println(s"time $updatedFinishTime: finished $vertex")
          edgesClassification.setFinishTime(vertex, updatedFinishTime)
          this.copy(finishTime = updatedFinishTime)

        def addEdge(u: V, v: V, firstTimeVisited: Boolean): TraversalState[V] =
          edgesClassification.addEdge(u, v, _ => firstTimeVisited)
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
        graph.adjacencies(vertex).foreach { neighbour =>
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
                  .adjacencies(vertex)
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
