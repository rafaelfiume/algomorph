package mars.rover

import mars.rover.Grid.{ Node, UnweightedGrid }

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Path {

  def shortestPath(start: Node, end: Node, grid: UnweightedGrid): List[Node] = {
    type Parents = Map[Node, Node]

    def pathTo(node: Node, through: Parents): List[Node] = through.get(node) match {
      case None => List.empty
      case Some(parent) => parent :: pathTo(parent, through)
    }

    def nodesParent(nodes: List[Node], parent: Node): Parents = {
      nodes.foldLeft(Map.empty[Node, Node]) { case (parents, n) => parents.updated(n, parent) }
    }

    def neighbours(node: Node, grid: UnweightedGrid): List[Node] = grid
      .find { case (n, _) => n == node }
      .map (_._2)
      .getOrElse (List.empty)

    @tailrec
    def bdf(searched: List[Node],
            queue: Queue[Node],
            parents: Parents): Parents = {
      if (queue.isEmpty) return parents
      if (queue.head == end) return parents

      val (next, dequeued) = queue.dequeue
      val updatedSearched = next :: searched
      val nodeNeighbours = neighbours(next, grid).filterNot(n => updatedSearched.contains(n) || queue.contains(n))
      val neighboursParent = nodesParent(nodeNeighbours, parent = next)
      bdf(updatedSearched, dequeued.enqueueAll(nodeNeighbours), parents ++ neighboursParent)
    }

    val neighboursFromStart = neighbours(start, grid)
    val startNeighboursParent = nodesParent(neighboursFromStart, parent = start)

    val parents = bdf(List(start), Queue(neighboursFromStart: _*), startNeighboursParent)
    val pathToEnd = pathTo(end, through = parents)
    if (pathToEnd.isEmpty) List.empty else (end :: pathToEnd).reverse
  }

}
