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
   * ===Complexity===
   *   - Time: Θ(size)
   *   - Space: Θ(size)
   */
  def fill[A](size: Int)(value: A): List[A] =
    require(size > 0, "size must be > 0")
    unfoldr[A, Int](size) { s => if s > 0 then Some((value, s - 1)) else None }

  /**
   * ===Complexity===
   *   - Time: Θ(n)
   *   - Space: Θ(1) - tail recursive via `foldl`
   */
  def size[A](numbers: List[A]): Int = foldl(numbers, 0) { (acc, _) => acc + 1 }

  /**
   * ===Complexity===
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
   * ===Complexity===
   *   - Time: Θ(1)
   *   - Space: Θ(1)
   */
  def head[A](list: List[A]): A = list match
    case Nil        => throw IllegalArgumentException("empty list")
    case Cons(h, _) => h

  /**
   * ===Complexity===
   *   - Time: Θ(1)
   */
  def tail[A](list: List[A]): List[A] = list match
    case Nil           => Nil
    case Cons(_, tail) => tail

  /**
   * ===Complexity===
   *   - Time: Θ(1)
   */
  def setHead[A](list: List[A], newHead: A): List[A] = list match
    case Nil           => Nil
    case Cons(_, tail) => Cons(newHead, tail)

  /**
   * ===Complexity===
   *   - Time: Θ(n) - where n = length of `prefix`
   *   - Space: Θ(n) - Θ(1) shares `suffix` structure + Θ(n) new cells
   */
  def append[A](prefix: List[A], suffix: List[A]): List[A] = foldr(prefix, suffix)(Cons(_, _))

  /**
   * ===Complexity===
   *
   * Let n = size of outer list Let m = length of longest inner list
   *
   *   - Time: Θ(n²m)
   *   - Space: Θ(n * m) - creates a new list
   *
   * Performance Notes:
   *   - The sequence of `append` operations lead to quadratic worst-case time.
   *   - For n lists of length m: 0 + m + 2m + ... (n-1)m = m(n(n -1)/2) = Θ(n²m).
   */
  def flatten[A, B](lists: List[List[A]]): List[A] = foldl(lists, Nil)(append)

  /**
   * ===Complexity===
   *   - Time: Θ(n)
   *   - Space: Θ(n) - creates a new list
   */
  def reverse[A](list: List[A]): List[A] = foldl(list, Nil) { (acc, x) => Cons(x, acc) }

  /**
   * Transforms `list` by appliying `f` to each of its elements.
   *
   * ===Complexity===
   *   - Time: Θ(n) - where `n` is the size of the list
   *   - Space: Θ(n) - creates a new list with size `n`
   */
  def map[A, B](list: List[A])(f: A => B): List[B] =
    foldr(list, Nil) { (a, b) => Cons(f(a), b) }

  /**
   * ===Complexity===
   *   - Time: Θ(n)
   *   - Space: Θ(n)
   */
  def filter[A](list: List[A])(f: A => Boolean): List[A] =
    foldr(list, Nil) { (a, b) => if f(a) then Cons(a, b) else b }

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
   * ===Evaluation Semantics===
   * `foldl(List(1, 2, 3), 0)(_ - _)` executes as:
   * {{{
   * ((0 - 1) - 2) - 3 = -6
   * }}}
   *
   * ===Recommended For===
   *   - Aggregation: sum, product.
   *
   * ===Complexity===
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
   * ===Evaluation Semantics===
   * `foldr(List(1, 2, 3), 0)(_ - _)` executes as:
   * {{{
   * 1 - (2 - (3 - 0)) = 2
   * }}}
   *
   * ===Recommended For===
   *   - Constructing new lists: map, filter.
   *   - Short-circuiting operations: exists, forall, foreach.
   *   - Right-associative computations: parsing, tree folds.
   *
   * ===Stack Safety Guarantees===
   * Unlike standard recurions, this implementation uses trampolined continuation-passing style (CPS):
   *   - Heap allocated continuations (pending operations) instead of stack frames during 'construction' phase.
   *   - `TailRec` to prevent stack overflow during the 'evaluation' phase.
   *   - \@see [TailCalls](https://www.scala-lang.org/api/current/scala/util/control/TailCalls$.html)
   *   - \@see [Stackless Scala With Free Monads](https://blog.higher-order.com/assets/trampolines.pdf)
   *
   * ===Complexity===
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
   * ===Complexity===
   *   - Time: Θ(n) - where n is the size of the resulting list
   *   - Space: Θ(n) - where n is the size of the resulting list
   */
  def unfoldr[A, B](z: B)(f: B => Option[(A, B)]): List[A] =
    @tailrec
    def loop(acc: List[A], state: B): List[A] = f(state) match
      case None                    => acc
      case Some((value, newState)) => loop(Cons(value, acc), newState)
    reverse(loop(Nil, z))

  /**
   * ===Complexity===
   *
   * Let n = length of `list``, and m = the average length of the list produced by `f`. Then:
   *   - Time: Θ(n * m)
   *   - Space: Θ(n * m)
   */
  def flatMap[A](list: List[A])(f: A => List[A]): List[A] =
    foldr(list, Nil) { (a, b) => append(f(a), b) }

  /*
   * ===Complexity===
   *  - Time: Θ(n)
   */
  def sum(list: List[Int]): Int = foldl(list, 0)(_ + _)

  /*
   * ===Complexity===
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

  /**
   * Removes duplicates preserving their original order of occurrence.
   *
   * ===Complexity===
   *   - Time: Θ(n) - where n = length of input `list`
   *   - Space: Θ(n) - for both the resulting list and `Set` storing elements.
   */
  def removeDups[A](list: List[A]): List[A] =
    @tailrec
    def loop(remaining: List[A], acc: List[A], seen: Set[A]): List[A] =
      remaining match
        case Nil                    => reverse(acc)
        case Cons(x, xs) if seen(x) => loop(xs, acc, seen)
        case Cons(x, xs)            => loop(xs, Cons(x, acc), seen + x)

    loop(list, Nil, Set.empty[A])

  /**
   * Finds the nth element from the end (1-based index).
   *
   * ===Evaluation Semantics===
   * {{{
   * nthToLast(List(A, B, C, D), k = 2):
   * Initial:
   *   fast = [A, B C, D], slow = [A, B, C, D]
   * After advance(fast, k = 2):
   *   fast = [C, D], slow = [A, B, C, D]
   * Make pointers sync:
   *   fast = [D], slow = [B, C, D]
   *   fast = [] , slow = [C, D] -> Return C
   * }}}
   *
   * ===Complexity===
   *   - Time: Θ(n)
   *   - Space: Θ(1)
   *
   * It uses the two-pointers technique to achieve Θ(1) space.
   */
  def nthToLast[A](list: List[A], k: Int): A =
    require(size(list) > 0, "list must not be empty")
    require(k > 0 && k <= size(list), s"k ($k) must be in 1..${size(list)}")

    @tailrec
    def advance(fast: List[A], steps: Int): List[A] = fast match
      case Cons(_, tail) if steps == 1 => tail
      case Cons(_, tail)               => advance(tail, steps - 1)
      case Nil                         => throw new IllegalStateException("fast pointer overflow")

    @tailrec
    def find(fast: List[A], slow: List[A]): A = (fast, slow) match
      case (Nil, Cons(head, _))                   => head
      case (Cons(_, fastTail), Cons(_, slowTail)) => find(fastTail, slowTail)
      case (_, _)                                 => throw new IllegalStateException("pointers not in sync")

    find(advance(list, k), list)

  /**
   * Partitions a list into elements `<` or `>=` the `pivot`.
   *
   * This algorithm is stable in the sense that the order of the elements is preserved other than the necessary moves around the
   * pivot.
   *
   * ===Evaluation Semantics===
   * {{{
   * partition(List(9, 3, 14), 5):
   * - Iter 1: left=[], right=[9]
   * - Iter 2: left=[3], right=[9]
   * - Iter 3: left=[3], right[14, 9]
   * - Merge: reverse(left) ++ reverse(right) -> List(3, 9, 14)
   * }}}
   *
   * ===Complexity===
   *   - Time: Θ(n)
   *   - Space: Θ(n)
   */
  def partition[A: Ordering](list: List[A], pivot: A): List[A] =
    import Ordering.Implicits.*

    @tailrec
    def loop(remaining: List[A], left: List[A], right: List[A]): List[A] =
      remaining match
        case Nil                              => append(reverse(left), reverse(right))
        case Cons(head, tail) if head < pivot => loop(tail, Cons(head, left), right)
        case Cons(head, tail)                 => loop(tail, left, Cons(head, right))
    loop(list, Nil, Nil)

  /**
   * Adds two numbers represented as reversed-digit liked lists.
   *
   * ===Digit Representation===
   * Numbers are stored in least-significant digit first (LSB):
   * {{{
   * [9, 8, 7] represents 789
   * [4, 6, 9] represents 964
   * }}}
   *
   * ===Semantic Evaluation===
   * Input: [9, 8, 7] + [4, 6, 9]
   *
   * Add digits pairwise with carry:
   *   - 9 + 4 = 13 -> digit=3, carry=1, sum=[3]
   *   - 8 + 6 + 1 = 15 -> digit=5, carry=1, sum=[5, 3]
   *   - 7 + 9 + 1 = 17 -> digit=7, carry=1, sum=[7, 5, 3]
   *   - sum=[1, 7, 5, 3]
   *
   * Reverse sum: [3, 5, 7, 1]
   *
   * ===Real-World Use Cases===
   *   - Arbitrary-precision arithmetic (beyond 64-bits)
   *   - Cryptographic Operations
   *
   * ===Complexity===
   *
   * Let n = length of left, and m = length of right. Then:
   *   - Time: Θ(max(n, m))
   *   - Space: Θ(max(n, m))
   */
  def sum(left: List[Int], right: List[Int]): List[Int] =
    @tailrec
    def validate(digits: List[Int]): Unit = digits match
      case Nil                          => ()
      case Cons(h, _) if h < 0 || h > 9 => throw IllegalArgumentException(s"digit $h must be between 0-9")
      case Cons(_, tail)                => validate(tail)

    @tailrec
    def loop(acc: List[Int], l: List[Int], r: List[Int], carry: Int): List[Int] = (l, r) match
      case (Nil, Nil) =>
        if carry > 0 then Cons(carry, acc) else acc
      case (Cons(lh, ltail), Cons(rh, rtail)) =>
        val sum = carry + lh + rh
        loop(Cons(sum % 10, acc), ltail, rtail, sum / 10)
      case (Nil, Cons(rh, rtail)) =>
        val sum = carry + rh
        loop(Cons(sum % 10, acc), l, rtail, sum / 10)
      case (Cons(lh, ltail), Nil) =>
        val sum = carry + lh
        loop(Cons(sum % 10, acc), ltail, r, sum / 10)

    validate(left)
    validate(right)
    reverse(loop(Nil, left, right, 0))

  /**
   * Checks if a list is a palindrome (reads the same backwards).
   *
   * ===Algorithm===
   *   1. Push first half of the list into a stack.
   *   1. Compare stack with second half of the list, skipping middle element for odd lengths.
   *
   * ===Complexity===
   *   - Time: Θ(n)
   *   - Space: Θ(n)
   */
  def isPalindrome[A](list: List[A]): Boolean =
    val length = size(list)
    val isEven = length % 2 == 0
    val pivot = length / 2

    @tailrec
    def stackFirstHalf(stack: List[A], remaining: List[A], steps: Int): (List[A], List[A]) =
      (remaining, steps) match
        case (_, 0) if isEven    => stack -> remaining
        case (_, 0) if !isEven   => stack -> tail(remaining)
        case (Cons(h, tail), st) => stackFirstHalf(Cons(h, stack), tail, st - 1)
        case (Nil, _)            => throw AssertionError("should consume only lalf of the list")

    @tailrec
    def compare(stack: List[A], secondHalf: List[A]): Boolean = (stack, secondHalf) match
      case (Nil, Nil) => true
      case (Cons(stackHead, stackTail), Cons(halfHead, tailHead)) =>
        if stackHead != halfHead then false
        else compare(stackTail, tailHead)
      case _ => throw AssertionError("lists not in sync")

    val (stack, secondHalf) = stackFirstHalf(Nil, list, pivot)
    compare(stack, secondHalf)

  /**
   * Finds the first shared node between two lists (by reference).
   *
   * ===Complexity===
   *
   * Let n = length of left, and m = length of right. Then:
   *   - Time: Θ(m + n)
   *   - Space: Θ(1) - only uses pointers
   */
  def findIntersection[A](left: List[A], right: List[A]): Option[List[A]] =
    @tailrec
    def advance(list: List[A], steps: Int): List[A] =
      if steps <= 0 then list
      else
        list match
          case Cons(head, tail) => advance(tail, steps - 1)
          case Nil              => throw AssertionError("advancing an empty list violates invariants")

    @tailrec
    def findCommon(l: List[A], r: List[A]): Option[List[A]] =
      (l, r) match
        case (Nil, Nil)                       => None
        case (a, b) if a eq b                 => Some(a)
        case (Cons(_, ltail), Cons(_, rtail)) => findCommon(ltail, rtail)
        case _                                => throw AssertionError("lists not in sync")

    val llength = size(left)
    val rlength = size(right)
    val diff = Math.abs(llength - rlength)
    val (shorter, longer) = if llength < rlength then left -> right else right -> left
    findCommon(shorter, advance(longer, diff))
