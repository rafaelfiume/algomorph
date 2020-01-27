package fp.ds

import scala.annotation.tailrec

object Lists {

  // ch02
  def frequency[A](ls: List[A]): Map[A, Int] = ls groupBy(x => x) map { case (w, agg) => w -> agg.length }

  // ch03
  def setElem[A](ls: List[A], n: Int, a: A): List[A] = {
    @tailrec
    def doSetElem(ls: List[A], n: Int, a: A, acc: List[A]): List[A] = ls match {
      case Nil => acc.reverse
      case _ :: xs if n == 0 => doSetElem(xs, n-1, a, a::acc)
      case x :: xs => doSetElem(xs, n-1, a, x::acc)
    }
    doSetElem(ls, n, a, List.empty)
  }

  def setElemWithStackOverflowIssues[A](ls: List[A], n: Int, a: A): List[A] = ls match {
    case Nil => Nil
    case _ if n < 0 => ls
    case _ :: xs if n == 0 => a :: xs
    case x :: xs => x :: setElemWithStackOverflowIssues(xs, n - 1, a)
  }

}
