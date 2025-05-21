package data

import scala.annotation.tailrec
import scala.util.control.TailCalls.*

/**
 * A recursive algebraid data type representing an immutable singly-linked list.
 *
 * Performance notes:
 *   - There are a few function implementations that look neat - from a readability perspective,
 * although that is opinionated - but are unreasonably slow in terms of performace.
 *   - See: `append`, `flatten` and `flatMap`.
 */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List:
  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def empty[A]: List[A] = Nil

  /**
   * @throws IllegalArgumentException
   *   if `size` <= 0
   *
   * Complexity:
   *   - Time: Θ(size)
   *   - Space: Θ(size)
   */
  def fill[A](size: Int)(value: A): List[A] =
    require(size > 0, "size must be > 0")
    unfoldr[A, Int](size) { s => if s > 0 then Some((value, s - 1)) else None }

  /**
   * Complexity:
   *   - Time: Θ(n)
   *   - Space: Θ(1) - tail recursive via `foldl`
   */
  def size[A](numbers: List[A]): Int = foldl(numbers, 0) { (acc, _) => acc + 1 }

  /**
   * @throws NoSuchElementException
   *   if list is empty
   *
   * Complexity:
   *   - Time: Θ(n)
   *   - Space: Θ(n)
   */
  def init[A](list: List[A]): List[A] =
    @tailrec
    def doInit(original: List[A], acc: List[A]): List[A] = original match
      case Nil          => throw new NoSuchElementException("init of empty list")
      case Cons(_, Nil) => acc
      case Cons(x, xs)  => doInit(xs, Cons(x, acc))
    reverse(doInit(list, Nil))

  /**
   * Complexity:
   *   - Time: Θ(1)
   */
  def tail[A](list: List[A]): List[A] = list match
    case Nil           => Nil
    case Cons(_, tail) => tail

  /**
   * Complexity:
   *   - Time: Θ(1)
   */
  def setHead[A](list: List[A], newHead: A): List[A] = list match
    case Nil           => Nil
    case Cons(_, tail) => Cons(newHead, tail)

  /**
   * Complexity:
   *   - Time: Θ(n) - where n = length of `prefix`
   *   - Space: Θ(n) - Θ(1) shares `suffix` structure + Θ(n) new cells
   */
  def append[A](prefix: List[A], suffix: List[A]): List[A] = foldr(prefix, suffix)(Cons(_, _))

  /**
   * Complexity:
   *
   * Time:
   *   - Let n = size of outer list
   *   - Let m = length of longest inner list
   *   - Θ(n²m)
   *
   * Space: Θ(n * m) - creates a new list.
   *
   * Performance Note:
   *   - The sequence of `append` operations lead to quadratic worst-case time.
   *   - For n lists of length m: 0 + m + 2m + ... (n-1)m = m(n(n -1)/2) = Θ(n²m).
   */
  def flatten[A, B](lists: List[List[A]]): List[A] = foldl(lists, Nil)(append)

  /**
   * Complexity:
   *   - Time: Θ(n)
   *   - Space: Θ(n) - creates a new list
   */
  def reverse[A](list: List[A]): List[A] = foldl(list, Nil) { (acc, x) => Cons(x, acc) }

  /**
   * Transforms `list` by appliying `f` to each of its elements.
   *
   * Complexity:
   *   - Time: Θ(n) - where `n` is the size of the list
   *   - Space: Θ(n) - creates a new list with size `n`
   */
  def map[A, B](list: List[A])(f: A => B): List[B] =
    foldr(list, List.empty[B]) { (a, b) => Cons(f(a), b) }

  /**
   * Complexity:
   *   - Time: Θ(n)
   *   - Space: Θ(n)
   */
  def filter[A](list: List[A])(f: A => Boolean): List[A] =
    foldr(list, List.empty[A]) { (a, b) => if f(a) then Cons(a, b) else b }

  @tailrec
  def drop[A](ns: List[A], n: Int): List[A] =
    if n <= 0 then return ns
    ns match
      case Nil         => Nil
      case Cons(_, xs) => drop(xs, n - 1)

  def dropWhile[A](ns: List[A], f: A => Boolean): List[A] = ns match
    case Nil                 => Nil
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _                   => ns

  /**
   * Folds a list from left to right using tail recursion with strict (immediate) evaluation.
   *
   * Evaluation Semantics: `foldl(List(1, 2, 3), 0)(_ - _)` executes as:
   * {{{
   * ((0 - 1) - 2) - 3 = -6
   * }}}
   *
   * Recommended For:
   *   - Aggregation: sum, product.
   *
   * Complexity:
   *   - Time: Θ(n)
   *   - Space: Θ(1) - tail recursive
   */
  @tailrec
  def foldl[A, B](list: List[A], z: B)(f: (B, A) => B): B = list match
    case Nil        => z
    case Cons(h, t) => foldl(t, f(z, h))(f)

  /**
   * Folds a list from right to left with delayed evaluation in `f`.
   *
   * Evaluation Semantics: `foldr(List(1, 2, 3), 0)(_ - _)` executes as:
   * {{{
   * 1 - (2 - (3 - 0)) = 2
   * }}}
   *
   * Recommended For:
   *   - Constructing new lists: map, filter.
   *   - Short-circuiting operations: exists, forall, foreach.
   *   - Right-associative computations: parsing, tree folds.
   *
   * Stack Safety Guarantees: Unlike standard recurions, this implementation uses trampolined continuation-passing style (CPS):
   *   - Heap allocated continuations (pending operations) instead of stack frames during 'construction' phase.
   *   - `TailRec` to prevent stack overflow during the 'evaluation' phase.
   *
   * @see
   *   [TailCalls](https://www.scala-lang.org/api/current/scala/util/control/TailCalls$.html)
   * @see
   *   [Stackless Scala With Free Monads](https://blog.higher-order.com/assets/trampolines.pdf)
   *
   * Complexity:
   *   - Time: Θ(n)
   *   - Space: Θ(n) heap - continuation; Θ(1) stack
   *
   * Performance Notes:
   *   - `foldr` trades stack space with heap allocation.
   *   - CPS and Trampoline add a constant factor overhead.
   */
  def foldr[A, B](list: List[A], z: B)(f: (A, B) => B): B =
    @tailrec
    def loop(acc: List[A], cont: B => TailRec[B]): TailRec[B] =
      acc match
        case Nil         => cont(z)
        case Cons(x, xs) => loop(xs, b => tailcall(cont(f(x, b))))
    loop(list, b => done(b)).result

  /**
   * Generates a list from the seed `z` by applying `f` until it returns `None`.
   *
   * Complexity:
   *   - Time: Θ(n) - where n is the size of the resulting list
   *   - Space: Θ(n) - where n is the size of the resulting list
   */
  def unfoldr[A, B](z: B)(f: B => Option[(A, B)]): List[A] =
    @tailrec
    def loop(acc: List[A], state: B): List[A] = f(state) match
      case None                    => acc
      case Some((value, newState)) => loop(Cons(value, acc), newState)
    reverse(loop(List.empty[A], z))

  /**
   * Complexity:
   *   - Let n = length of `list``, and m = the average length of the list produced by `f`
   *   - Time: Θ(n * m)
   *   - Space: Θ(n * m)
   */
  def flatMap[A](list: List[A])(f: A => List[A]): List[A] =
    foldr(list, List.empty[A]) { (a, b) => append(f(a), b) }

  /*
   * Complexity:
   *  - Time: Θ(n)
   */
  def sum(list: List[Int]): Int = foldl(list, 0)(_ + _)

  /*
   * Complexity:
   *  - Time: Θ(n)
   */
  def multiply(list: List[Int]): Int = foldl(list, 1)(_ * _)

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
