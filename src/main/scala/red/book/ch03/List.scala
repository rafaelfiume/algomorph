package red.book.ch03

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil         => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(ds: List[Double]): Double = ds match
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](ns: List[A]): List[A] = ns match
    case Nil         => Nil
    case Cons(_, xs) => xs

  def setHead[A](ns: List[A], n: A): List[A] = ns match
    case Nil         => Nil
    case Cons(_, xs) => Cons(n, xs)

  def drop[A](ns: List[A], n: Int): List[A] =
    if n <= 0 then return ns
    ns match
      case Nil         => Nil
      case Cons(_, xs) => drop(xs, n - 1)

  def dropWhile[A](ns: List[A], f: A => Boolean): List[A] = ns match
    case Nil                 => Nil
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _                   => ns

  def init2[A](ns: List[A]): List[A] =
    def doInit(original: List[A], acc: List[A]): List[A] = original match
      case Nil          => sys.error("empty list")
      case Cons(_, Nil) => acc
      case Cons(x, xs)  => Cons(x, doInit(xs, acc))
    doInit(ns, Nil)

  def init[A](list: List[A]): List[A] = list match
    case Nil          => sys.error("init from empty list")
    case Cons(_, Nil) => Nil
    case Cons(x, xs)  => Cons(x, init(xs))

  def shortCircuitProduct(numbers: List[Int]): Int =
    // We can't short-circuit product using foldRight cause the control flow is delegated to that function.
    // We could have a specialized short-circuit of foldRight that would enable us to stop iteration in a certain condition is met
    foldRight(numbers, 1)(_ * _)

  // Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
  // 1 + (2 + (3 + (4)))
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))

  // Cons(4, Cons(3, Cons(2, Cons(1, Nil))))
  // 4 + (3 + (2 + 1))
  @tailrec
  def foldLeft[A, B](list: List[A], z: B)(f: (B, A) => B): B = list match
    case Nil        => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)

  def foldRight_l1[A, B](list: List[A], z: B)(f: (A, B) => B): B = foldLeft(myReverse(list), z)((a, b) => f(b, a))

  def foldRight_l2[A, B](list: List[A], z: B)(f: (A, B) => B): B = ???

  def foldLeft_r1[A, B](list: List[A], z: B)(f: (B, A) => B): B = ???

  def myLength[A](list: List[A]): Int = foldRight(list, 0)((_, acc) => acc + 1)

  def sumLeft(numbers: List[Int]): Int = foldLeft(numbers, 0)(_ + _)
  def productLeft(numbers: List[Int]): Int = foldLeft(numbers, 1)(_ * _)
  def lengthLeft[A](numbers: List[A]): Int = foldLeft(numbers, 0)((acc, _) => acc + 1)

  def myReverse[A](list: List[A]): List[A] = foldLeft(list, Nil: List[A])((acc, x) => Cons(x, acc))
  def myReverse_l1[A](list: List[A]): List[A] = foldLeft_r1(list, Nil: List[A])((acc, x) => Cons(x, acc))

  // foldRight(Cons(1, Cons(2, Cons(3, Nil)), Cons(4, Cons(5, Cons(6, Nil))) )(Cons(_,_))
  // Cons(1, foldRight(Cons(2, Cons(3, Nil)), Cons(4, Cons(5, Cons(6, Nil))) )(Cons(_,_))
  // Cons(1, Cons(2, foldRight(Cons(3), Nil)), Cons(4, Cons(5, Cons(6, Nil))) )(Cons(_,_))
  // Cons(1, Cons(2, Cons(3)), foldRight(Nil, Cons(4, Cons(5, Cons(6, Nil))) )(Cons(_,_))
  // Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Nil))))
  def myappend[A](l: List[A], r: List[A]): List[A] = foldRight(l, r)(Cons(_, _))
  def myappendWithSomeTrace[A](l: List[A], r: List[A]): List[A] = foldRight(l, r)((a, b) =>
    println(s"a: $a; b: $b"); Cons(a, b)
  )

  def concat[A, B](xxs: List[List[A]]): List[A] = foldLeft(xxs, Nil: List[A])(myappend)

  def plus1(nums: List[Int]): List[Int] = foldRight(nums, Nil: List[Int])((a, b) => Cons(a + 1, b))

  def doubleToString(floats: List[Double]): List[String] = foldRight(floats, Nil: List[String])((a, b) => Cons(a.toString, b))

  def map[A, B](list: List[A])(f: A => B): List[B] = foldRight(list, Nil: List[B])((a, b) => Cons(f(a), b))

  def filter[A](list: List[A])(f: A => Boolean): List[A] = foldRight(list, Nil: List[A]) { (a, b) =>
    if f(a) then Cons(a, b) else b
  }

  def flatMap[A](list: List[A])(f: A => List[A]): List[A] = foldRight(list, Nil: List[A])((a, b) => myappend(f(a), b))

  def filterWithFlatMap[A](list: List[A])(f: A => Boolean): List[A] = flatMap(list)(a => if f(a) then List(a) else Nil)

  def addPairWise(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match
    case (Nil, _)                     => Nil
    case (_, Nil)                     => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairWise(t1, t2))

  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = (l1, l2) match
    case (Nil, _)                     => Nil
    case (_, Nil)                     => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))

  @tailrec
  def myStartWith[A](a: List[A], b: List[A]): Boolean = (a, b) match
    case (_, Nil)                                 => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => myStartWith(t1, t2)
    case _                                        => false

  @tailrec
  def hasSubsequence[A](a: List[A], b: List[A]): Boolean = a match
    case Nil                    => b == Nil
    case _ if myStartWith(a, b) => true
    case Cons(_, t)             => hasSubsequence(t, b)
