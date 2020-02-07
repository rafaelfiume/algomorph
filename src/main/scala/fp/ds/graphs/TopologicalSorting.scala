package fp.ds.graphs

object TopologicalSorting {

  type Task = String
  type TaskPrecedence = (Task, Task)

  def topsort(tasks: List[TaskPrecedence]): List[Task] = {
    def neighbours(task: Task): List[Task] = tasks.filter(t => t._1 == task).map(_._2)

    def sort(stack: List[Task], path: List[Task], visited: List[Task]): List[Task] = stack match {
      case Nil                                  => visited
      case next :: _ if path.contains(next)     => throw new IllegalArgumentException(s"detected cycle at $next")
      case next :: xs if visited.contains(next) => sort(xs, path, visited)
      case next :: xs                           => sort(xs, path, next :: sort(neighbours(next), next :: path, visited))
    }

    sort(List(tasks.head._1), Nil, Nil)
  }

}

/*
 * Topological sort in a graph is implemented using a variation of depth-first search.
 * Here's a nice explanation about it: https://en.wikipedia.org/wiki/Topological_sorting
 */
