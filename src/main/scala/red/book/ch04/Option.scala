package red.book.ch04

import scala.collection.immutable.List.empty
import scala.{Either as _, None as _, Option as _, Some as _}

sealed trait Option[+A]:

  def get: A
  def isEmpty: Boolean
  def nonEmpty: Boolean = !isEmpty

  def map[B](f: A => B): Option[B] = this match
    case None    => None
    case Some(v) => Some(f(v))

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match
    case None    => default
    case Some(v) => v

  def orElse[B >: A](default: => Option[B]): Option[B] = map(Some(_)).getOrElse(default)

  def filter(p: A => Boolean): Option[A] = flatMap(v => if p(v) then Some(v) else None)

  def fold[B](ifEmpty: => B)(f: A => B): B = this match
    case None    => ifEmpty
    case Some(v) => f(v)

case class Some[A](get: A) extends Option[A]:
  override def isEmpty: Boolean = false

case object None extends Option[Nothing]:
  override def get: Nothing = throw new NoSuchElementException("None.get")
  override def isEmpty: Boolean = true

object Option:

  def apply[A](v: A): Option[A] = if v == null then None else Some(v)

  def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap { m =>
      mean(xs.map(x => math.pow(x - m, 2)))
    }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(v1 => b.map(v2 => f(v1, v2)))

  def traverse[A, B](list: List[A])(f: A => Option[B]): Option[List[B]] =
    list.foldRight(some(empty[B]))((x, xs) => map2(f(x), xs)(_ :: _))

  def sequence[A](list: List[Option[A]]): Option[List[A]] = list.foldRight(some(empty[A]))((x, xs) => map2(x, xs)(_ :: _))

  def sequence_via_traverse[A](list: List[Option[A]]): Option[List[A]] = traverse(list)(x => x)

  def sequence2[A](list: List[Option[A]]): Option[List[A]] = list.foldRight(some(empty[A])) { (o, list) =>
    o.flatMap(x => list.flatMap(xs => Some(x :: xs)))
  }

  def sequence3[A](list: List[Option[A]]): Option[List[A]] = list.foldRight(some(empty[A]): Option[List[A]]) { (o, list) =>
    (o, list) match
      case (None, _)           => None
      case (_, None)           => None
      case (Some(x), Some(xs)) => Some(x :: xs)
  }

  //
  //// Helper
  //

  def some[A](v: A): Option[A] = Some(v)

  def none[A]: Option[A] = None
