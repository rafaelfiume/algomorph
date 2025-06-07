package graphs

import graphs.Graph.Vertex

object Path:
  def reconstruct[V <: Vertex](parents: Map[V, V], start: V, end: V): List[V] =
    @annotation.tailrec
    def loop(end: V, acc: List[V]): List[V] =
      if start == end then start :: acc
      else
        parents.get(end) match
          case None         => Nil
          case Some(parent) => loop(parent, end :: acc)

    loop(end, Nil)
