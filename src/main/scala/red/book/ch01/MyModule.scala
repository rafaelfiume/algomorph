package red.book.ch01

object MyModule:

  val fibSequence: Int => List[Int] = n =>
    @annotation.tailrec
    def fibAcc(n: Int, acc: List[Int]): List[Int] =
      (n, acc) match
        case (0, l)                 => l
        case (n, Nil)               => fibAcc(n - 1, List(0))
        case (n, l @ 0 :: _)        => fibAcc(n - 1, 1 :: l)
        case (n, l @ x1 :: x2 :: _) => fibAcc(n - 1, x1 + x2 :: l)
        case v                      => throw new RuntimeException(s"ops! Unexpected value $v")

    fibAcc(n, List.empty)

  val fib: Int => Int = n => fibSequence(n).head
