package red.book.ch05

import red.book.ch04._
import red.book.ch05.Stream.{ cons, empty, unfold }

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.{ Either => _, None => _, Option => _, Some => _ }

sealed trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  final def take(n: Int): Stream[A] = this match {
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case _ => empty // TODO test this
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => empty
  }

  def takeWhile_w_fold(p: A => Boolean): Stream[A] = foldRight(empty[A])((x, acc) => if (p(x)) cons(x, acc) else empty) // my original solution used `else acc` instead

  def forAll(p: A => Boolean): Boolean = foldRight(true)((x, acc) => p(x) && acc)

  def headOption: Option[A] = foldRight[Option[A]](None)((x, _) => Some(x))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((x, acc) => cons(f(x), acc))

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((x, acc) => if (p(x)) cons(x, acc) else acc)

  def append[AA >: A](s: Stream[AA]): Stream[AA] = foldRight(s)((x, acc) => cons(x, acc))
  def ++[AA >: A](s: Stream[AA]): Stream[AA] = append(s)

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((x, acc) => f(x) append acc)

  def startsWith[B](other: Stream[B]): Boolean = zipAll(other) takeWhile(_._2.nonEmpty) forAll {
    case (a, b) => a == b
  }

  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case stream => Some(stream -> (stream drop 1))
  } append Stream(empty)

  def hasSubsequence[B](sub: Stream[B]): Boolean = tails exists(_ startsWith sub)

  def scanRight[AA >: A](z: AA)(op: (AA, AA) => AA): Stream[AA] = foldRight(empty[AA]) { // This is being strictly consumed
    case (x, Empty) => cons(op(z,x), empty)
    case (x, s@Cons(h, _)) => cons(op(x, h()), s)
    case v => throw new RuntimeException(s"ops! Unexpected value $v")
  } append Stream(z)

  def scanLeft[AA >: A](z: AA)(op: (AA, AA) => AA): Stream[AA] = Stream(z).append {
    unfold(z -> this) {
      case (acc, Cons(h, t)) => val newAcc = op(acc, h()); Some(newAcc -> (newAcc -> t()))
      case _ => None
    }
  }

  def toList: List[A] = {
    @tailrec
    def go(s: Stream[A], l: List[A]): List[A] = s match {
      case Empty => l
      case Cons(h, t) => go(t(), h()::l)
    }

    go(this, Nil) reverse
  }

  // 5.13 Using unfold...

  def map_u[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some(f(h()) -> t())
    case Empty => None
  }

  def take_u(n: Int): Stream[A] = unfold((n, this)) {
    case (i, Cons(h,t)) if i > 0 => Some(h() -> ((i-1) -> t()))
    case _ => None
  }

  def takeWhile_u(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h,t) if p(h()) => Some(h() -> t())
    case _ => None
  }

  def zipWith[B, C](other: Stream[B])(op: (A, B) => C): Stream[C] = unfold(this -> other) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(op(h1(), h2()) -> (t1() -> t2()))
    case _ => None
  }

  def zip[B](other: Stream[B]): Stream[(A, B)] = zipWith(other)((_,_))

  def zipAllWith[B,C](other: Stream[B])(op: (Option[A],Option[B]) => C): Stream[C] = unfold(this -> other) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(op(Some(h1()),Some(h2())) -> (t1() -> t2()))
    case (Empty, Cons(h2, t2)) => Some(op(None,Some(h2())) -> (Empty -> t2()))
    case (Cons(h1, t1), Empty) => Some(op(Some(h1()), None) -> (t1() -> Empty))
    case _ => None
  }

  def zipAll[B](other: Stream[B]): Stream[(Option[A], Option[B])] = zipAllWith(other)((_,_))
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = cons(a, tail)
    tail
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs(): Stream[Int] = {
    def go(v: Int, n: Int): Stream[Int] = cons(v, go(n, v+n))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((v, s)) => cons(v, unfold(s)(f))
  }

  def unfoldViaStreamFold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((a, s)) => cons(a, empty).foldRight(empty[A])((x, _) => cons(x, unfoldViaStreamFold(s)(f)))
  }

  def unfoldViaFold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z).fold (empty[A]) { case (a, s) => cons(a, unfold(s)(f)) }

  def unfoldViaMap[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) map { case (a,s1) => cons(a, unfoldViaMap(s1)(f)) } getOrElse empty
  }

  // Using unfold

  def fibs_u(): Stream[Int] = unfold((0, 1)) { case (f0, f1) => Some(f0 -> (f1 -> (f0+f1))) }

  def from_u(n: Int): Stream[Int] = unfold(n) { s => Some(s -> (s+1)) }

  def constant_u[A](a: A): Stream[A] = unfold(a)(_ => Some(a -> a))

  def ones_u(): Stream[Int] = unfold(1)(_ => Some(1 -> 1))

}