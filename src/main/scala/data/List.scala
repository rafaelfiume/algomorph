package data

import scala.annotation.tailrec
import scala.util.control.TailCalls.*

/*
 * A recursive algebraid data type representing an immutable singly-linked list.
 */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List:
  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def empty[A]: List[A] = Nil

  def fill[A](size: Int)(value: A): List[A] =
    require(size > 0, "size must be > 0")
    unfoldr[A, Int](size) { s => if s > 0 then Some((value, s - 1)) else None }

  /*
   * Time: O(n).
   * Space: O(1) - tail recursive via `foldLeft`.
   */
  def size[A](numbers: List[A]): Int = foldLeft(numbers, 0) { (acc, _) => acc + 1 }

  /*
   * Time complexity: O(n).
   * Space complexity: O(n).
   *
   * @throws NoSuchElementException if list is empty
   */
  def init[A](list: List[A]): List[A] =
    @tailrec
    def doInit(original: List[A], acc: List[A]): List[A] = original match
      case Nil          => throw new NoSuchElementException("init of empty list")
      case Cons(_, Nil) => acc
      case Cons(x, xs)  => doInit(xs, Cons(x, acc))
    reverse(doInit(list, Nil))

  /**
   * Time: O(1).
   */
  def tail[A](list: List[A]): List[A] = list match
    case Nil           => Nil
    case Cons(_, tail) => tail

  /**
   * Time: O(1).
   */
  def setHead[A](list: List[A], newHead: A): List[A] = list match
    case Nil           => Nil
    case Cons(_, tail) => Cons(newHead, tail)

  def append[A](l: List[A], r: List[A]): List[A] = foldRight(l, r)(Cons(_, _))

  /*
   * Time: O(L) where L is the total of elements across all lists.
   *   - Let n = the size of the outer list
   *   - Let m = the size of the longest inner list
   *   - Worst case: O(n * m) - when all inner lists are size m.
   *
   * Space: O(L) - creates a new list.
   */
  def concat[A, B](lists: List[List[A]]): List[A] = foldLeft(lists, Nil)(append)

  /*
   * Time: O(n).
   * Space: O(n) - creates a new list.
   */
  def reverse[A](list: List[A]): List[A] = foldLeft(list, Nil) { (acc, x) => Cons(x, acc) }

  def drop[A](ns: List[A], n: Int): List[A] =
    if n <= 0 then return ns
    ns match
      case Nil         => Nil
      case Cons(_, xs) => drop(xs, n - 1)

  def dropWhile[A](ns: List[A], f: A => Boolean): List[A] = ns match
    case Nil                 => Nil
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _                   => ns

  /*
   * Folds a list from left to right using tail recursion with strict (immediate) evaluation.
   *
   * === Evaluation Semantics ===
   * foldLeft(List(1, 2, 3), 0)(_ - _) executes as:
   * {{{
   * ((0 - 1) - 2) - 3 = -6
   * }}}
   *
   * === Recommended For ===
   * - Aggregation: sum, product.
   *
   * Time: O(n).
   * Space: O(1) - tail recursive.
   */
  @tailrec
  def foldLeft[A, B](list: List[A], z: B)(f: (B, A) => B): B = list match
    case Nil        => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)

  /*
   * Folds a list from right to left with delayed evaluation in `f`.
   *
   * === Evalunation Semantics ===
   * foldRight(List(1, 2, 3), 0)(_ - _) executes as:
   * {{{
   * 1 - (2 - (3 - 0)) = 2
   * }}}
   *
   * === Recommended For ===
   * - Constructing new lists: map, filter.
   * - Short-circuiting operations: exists, forall, foreach.
   * - Right-associative computations: parsing, tree folds.
   *
   * === Stack Safety Guarantees ===
   * Unlike standard recurions, this implementation uses trampolined continuation-passing style (CPS):
   * 1. Heap allocated continuations (pending operations) instead of stack frames during 'construction' phase.
   * 2. `TailRec` to prevent stack overflow during the 'evaluation' phase.
   *
   * @see [TailCalls](https://www.scala-lang.org/api/current/scala/util/control/TailCalls$.html)
   * @see [Stackless Scala With Free Monads](https://blog.higher-order.com/assets/trampolines.pdf)
   *
   * Time: O(n).
   * Space: O(1) - stack safe.
   */
  def foldRight[A, B](list: List[A], z: B)(f: (A, B) => B): B =
    @tailrec
    def loop(acc: List[A], cont: B => TailRec[B]): TailRec[B] =
      acc match
        case Nil         => cont(z)
        case Cons(x, xs) => loop(xs, b => tailcall(cont(f(x, b))))
    loop(list, b => done(b)).result

  def unfoldr[A, B](seed: B)(f: B => Option[(A, B)]): List[A] =
    @tailrec
    def loop(acc: List[A], state: B): List[A] = f(state) match
      case None                    => acc
      case Some((value, newState)) => loop(Cons(value, acc), newState)

    reverse(loop(List.empty[A], seed))

  def map[A, B](list: List[A])(f: A => B): List[B] = foldRight(list, List.empty[B])((a, b) => Cons(f(a), b))

  def filter[A](list: List[A])(f: A => Boolean): List[A] = foldRight(list, Nil: List[A]) { (a, b) =>
    if f(a) then Cons(a, b) else b
  }

  def flatMap[A](list: List[A])(f: A => List[A]): List[A] = foldRight(list, Nil: List[A])((a, b) => append(f(a), b))

  def filterWithFlatMap[A](list: List[A])(f: A => Boolean): List[A] = flatMap(list)(a => if f(a) then List(a) else Nil)

  /**
   * Time: O(n)
   */
  def sum(list: List[Int]): Int = foldLeft(list, 0)(_ + _)

  /**
   * Time: O(n)
   */
  def multiply(list: List[Int]): Int = foldLeft(list, 1)(_ * _)

  def sumPairWise(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match
    case (Nil, _)                     => Nil
    case (_, Nil)                     => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, sumPairWise(t1, t2))

  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = (l1, l2) match
    case (Nil, _)                     => Nil
    case (_, Nil)                     => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))

  @tailrec
  def startWith[A](a: List[A], b: List[A]): Boolean = (a, b) match
    case (_, Nil)                                 => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startWith(t1, t2)
    case _                                        => false

  @tailrec
  def hasSubsequence[A](a: List[A], b: List[A]): Boolean = a match
    case Nil                  => b == Nil
    case _ if startWith(a, b) => true
    case Cons(_, t)           => hasSubsequence(t, b)
