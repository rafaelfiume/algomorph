package mars.rover

import Grid.{ UnweightedEdges, Node }

object Grid {

  type Node = (Int, Int)

  type UnweightedEdges = List[Node]
  type UnweightedGrid = Seq[(Node, UnweightedEdges)]

  type Weight = Int
  case class Edge(node: Node, w: Weight) { override def toString: String = s"[n=$node;w=$w]" }
  type Edges = List[Edge]
  type Grid = Seq[(Node, Edges)]

  def unweighted(rows: Int, columns: Int, obstacles: List[Node] = Nil): UnweightedGrid = for {
    r     <- 0 until rows
    c     <- 0 until columns
    if !obstacles.contains((r,c))
    left  = (r, if (c-1 < 0) columns-1 else c-1)
    up    = (if (r-1 < 0) rows-1 else r-1, c)
    down  = (if (r+1 >= rows) 0 else r+1, c)
    right = (r, if (c+1 >= columns) 0 else c+1)
  } yield (r, c) -> UnweightedEdges(obstacles, left, up, down, right)

  def weighted(rows: Int, columns: Int, weights: List[Edge]): Grid = {
    unweighted(rows, columns, Nil) map { case (node, unweightedEdges) =>
      node -> unweightedEdges.map { n =>
        Edge(
          node = n,
          w = weights.find(_.node == n).map(_.w).getOrElse(1)
        )
      }
    }
  }

  def printPath(edges: List[Node]): String = edges.mkString(" -> ")

}

private object UnweightedEdges {
  def apply(obstacles: List[Node], edges: Node*): UnweightedEdges = obstacles match {
    case Nil => edges.toList
    case obs => edges.toList.filterNot(obs.contains(_))
  }

}
