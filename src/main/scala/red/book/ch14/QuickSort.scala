package red.book.ch14

object QuickSort:

  def swap[S](arr: STArray[S, Int], i: Int, j: Int): ST[S, Unit] = for
    x <- arr.read(i)
    y <- arr.read(j)
    _ <- arr.write(i, y)
    _ <- arr.write(j, x)
  yield ()

  private def partition[S](arr: STArray[S, Int], n: Int, r: Int, pivot: Int): ST[S, Int] = for
    _ <- arr.read(pivot)
    _ <- swap(arr, pivot, r)
    j = n
  // TODO OOOOOOOOOOo
  yield j

  private def qs[S](arr: STArray[S, Int], n: Int, r: Int): ST[S, Unit] = for
    pi <- partition(arr, n, r, n + (n - r) / 2)
    _ <- qs(arr, n, pi - 1)
    _ <- qs(arr, pi + 1, r)
  yield ()

  def quicksort(xs: List[Int]): List[Int] = if xs.isEmpty then xs
  else
    ST.runST(
      new RunnableST[List[Int]]:
        override def apply[S]: ST[S, List[Int]] = for
          arr <- STArray.fromList(xs)
          size <- arr.size
          _ <- qs(arr, 0, size - 1)
          sorted <- arr.freeze
        yield sorted
    )
