package cracking.ch02

import scala.collection.immutable.List.empty
import scala.collection.mutable

object LinkedLists:

  def removeDups[A](ls: List[A]): List[A] =
    ls match
      case Nil    => ls
      case h :: t => h :: removeDups(t).filter(_ != h)

  def removeDupsWithBuffer[A](ls: List[A]): List[A] =
    def go(l: List[A], elems: mutable.Set[A]): List[A] = l.foldLeft(empty[A]) { case (acc, e) =>
      if elems.contains(e) then acc
      else
        elems.add(e)
        e :: acc
    }
    go(ls, new mutable.HashSet[A]()).reverse

  def kthToLast[A](k: Int, ls: List[A]): List[A] =
    ls.reverse
      .foldLeft(empty[A] -> 0) { case ((l, p), e) =>
        if p < k then (e :: l) -> (p + 1)
        else l -> p
      }
      ._1

  def kthToLast2[A](k: Int, ls: List[A]): List[A] =
    var c = 0
    def go(k: Int, l: List[A]): List[A] =
      if l.isEmpty then return Nil

      val node = go(k, l.tail)
      c = c + 1
      if c == k then l else node

    go(k, ls)

  def removeNode[A](node: A, ls: List[A]): List[A] = ls.foldRight(empty[A]) { (e, acc) =>
    if e == node then acc else e :: acc
  }

  def partition[A](partition: A, ls: List[A])(implicit ord: Ordering[A]): List[A] =
    val (less, equalOrGreater) = ls.foldRight(empty[A] -> empty[A]) { case (e, (ll, lg)) =>
      if ord.lt(e, partition) then (e :: ll) -> lg else ll -> (e :: lg)
    }
    less ++ equalOrGreater
