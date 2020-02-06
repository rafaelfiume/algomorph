package mars.rover.dijkstra

import java.lang.Integer.MAX_VALUE

import mars.rover.Grid.{ Edges, Grid, Node }

import scala.annotation.tailrec

object Path {

  def shortestPath(start: Node, end: Node, grid: Grid): List[Node] = {
    type Costs = Map[Node, Int]
    type Parents = Map[Node, Node]

    val allNodes = grid.map(_._1)

    def smallestTentativeDistance(costs: Costs, visited: List[Node]): Option[Node] = {
      val nodesToVisit = allNodes.filter(n => !visited.contains(n))
      if (nodesToVisit.isEmpty) return None

      val nextCheapest = nodesToVisit.tail.foldLeft(nodesToVisit.head) { (currMin, node) =>
        val currMinCost = costs(currMin)
        val nextCost = costs(node)
        if (nextCost < currMinCost) node else currMin
      }
      Some(nextCheapest)
    }

    def neighbours(node: Node): Edges = grid
      .find(_._1 == node)
      .map(_._2)
      .getOrElse(Nil)

    def pathTo(node: Node, through: Parents): List[Node] = through.get(node) match {
      case None => Nil
      case Some(p) => p :: pathTo(p, through)
    }

    @tailrec
    def dijkstra(visited: List[Node], costs: Costs, parents: Parents): Parents = {
      if (visited.contains(end)) return parents

      smallestTentativeDistance(costs, visited) match {
        case None => parents

        case Some(current) =>
          val neighs = neighbours(current)
          val (updatedCosts, updatedParents) = neighs.foldLeft(costs -> parents) { case (accCosts -> accParents, e) =>
            val costToNode = costs(e.node)
            val newCostToNode = costs(current) + e.w
            if (newCostToNode < costToNode) {
              accCosts.updated(e.node, newCostToNode) -> accParents.updated(e.node, current)
            } else {
              accCosts -> accParents
            }
          }
          dijkstra(current :: visited, updatedCosts, updatedParents)
      }
    }

    val initialCost = allNodes.foldLeft(Map.empty[Node, Int]) { (costs, n) => costs.updated(n, MAX_VALUE) }.updated(start, 0)

    val parents = dijkstra(Nil, initialCost, Map.empty)
    val pathToEnd = pathTo(end, through = parents)
    if (pathToEnd.isEmpty) Nil else (end :: pathToEnd).reverse
  }
}

/*
 * A couple links describing the algorithm:
 *
 * - https://everythingcomputerscience.com/algorithms/Dijkstras_Algorithm.html
 * - https://learning.oreilly.com/library/view/grokking-algorithms-an/9781617292231/kindle_split_013.html
 */
