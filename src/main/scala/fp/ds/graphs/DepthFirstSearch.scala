package fp.ds.graphs

import scala.annotation.tailrec

object DepthFirstSearch {

  type Node = String
  type Edge = (Node, Node)
  type Graph = List[Edge]

  def traverse(start: Node, graph: Graph): List[Node] = {
    def neighbours(node: Node): List[Node] = graph.filter(edge => edge._1 == node).map(_._2)

    @tailrec
    def dfs(stack: List[Node], visited: List[Node]): List[Node] = stack match {
      case Nil                            => visited
      case x :: xs if visited.contains(x) => dfs(xs, visited)
      case x :: xs                        => dfs(neighbours(x) ++ xs, x :: visited)
    }

    dfs(List(start), Nil).reverse
  }
}

// Follows an implementation of dfs that avoids list appending, but that it's not tail safe
// def dfs(stack: List[Node], visited: List[Node]): List[Node] = stack match {
//   case Nil                            => visited
//   case x :: xs if visited.contains(x) => dfs(xs, visited)
//   case x :: xs                        => dfs(xs, dfs(neighbours(x), x :: visited))
// }