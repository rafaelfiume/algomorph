package fp.ds.graphs

object TopologicalSorting {

  type Task = String
  type TaskPrecedence = (Task, Task)

  def topsort(tasks: List[TaskPrecedence]): List[Task] = {
    def neighbours(task: Task): List[Task] = tasks.filter(t => t._1 == task).map(_._2)

    def sort(stack: List[Task], visited: List[Task]): List[Task] = stack match {
      case Nil => visited
      case x :: xs if visited.contains(x) => sort(xs, visited)
      case x :: xs => sort(xs, x :: sort(neighbours(x), visited))
    }

    sort(List(tasks.head._1), Nil)
  }
}

/*  Topological sort in a graph is implemented using a variation of depth-first search. */
