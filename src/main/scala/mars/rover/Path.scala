package mars.rover

import Grid.{ Grid, Node }

import scala.annotation.tailrec

object Path {

  def shortestPath(start: Node, end: Node, grid: Grid): List[Node] = {
    type Parents = Map[Node, Node]

    def pathTo(node: Node, through: Parents): List[Node] = through.get(node) match {
      case None => List.empty
      case Some(parent) => parent :: pathTo(parent, through)
    }

    def nodesParent(nodes: List[Node], parent: Node): Parents = {
      nodes.foldLeft(Map.empty[Node, Node]) { case (parents, n) => parents.updated(n, parent) }
    }

    def neighbours(node: Node, grid: Grid): List[Node] = grid
      .find { case (n, _) => n == node }
      .map (_._2)
      .getOrElse (List.empty)

    @tailrec
    def bdf(searched: List[Node],
            queue: List[Node],
            parents: Parents): Parents = queue match {
      case Nil => parents
      case x :: _ if x == end => parents
      case x :: xs =>
        val updatedSearched = x :: searched
        val nodeNeighbours = neighbours(x, grid).filterNot(n => updatedSearched.contains(n) || queue.contains(n))
        val neighboursParent = nodesParent(nodeNeighbours, parent = x)
        bdf(updatedSearched, xs ++ nodeNeighbours, parents ++ neighboursParent)
    }

    val neighboursFromStart = neighbours(start, grid)
    val startNeighboursParent = nodesParent(neighboursFromStart, parent = start)

    val parents = bdf(List(start), neighboursFromStart, startNeighboursParent)
    val pathToEnd = pathTo(end, through = parents)
    if (pathToEnd.isEmpty) List.empty else (end :: pathToEnd).reverse
  }

}
