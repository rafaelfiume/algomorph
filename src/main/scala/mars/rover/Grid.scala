package mars.rover

import Grid.{ Edges, Node }

object Grid {

  type Node = (Int, Int)
  type Edges = List[Node]
  type Grid = Seq[(Node, Edges)]

  def apply(rows: Int, columns: Int, obstacles: List[Node] = Nil): Grid = for {
      r     <- 0 until rows
      c     <- 0 until columns
      if !obstacles.contains((r,c))
      left  = (r, if (c-1 < 0) columns-1 else c-1)
      up    = (if (r-1 < 0) rows-1 else r-1, c)
      down  = (if (r+1 >= rows) 0 else r+1, c)
      right = (r, if (c+1 >= columns) 0 else c+1)
    } yield (r, c) -> Edges(List(left, up, down, right), obstacles)
}

object Edges {
  def apply(edges: Edges, obstacles: List[Node]): Edges = obstacles match {
    case Nil => edges
    case obs => edges.filterNot(obs.contains(_))
  }

  def printPath(edges: Edges): String = edges.mkString(" -> ")
}
