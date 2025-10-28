package data.tree

import Ordering.Implicits.*

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree:

  def unit[A](a: A): Tree[A] = Leaf(a)

  def mySize[A](tree: Tree[A]): Int = tree match
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + mySize(l) + mySize(r)

  def maximum[A](tree: Tree[A])(implicit o: A => Ordered[A]): A = tree match
    case Leaf(v)      => v
    case Branch(l, r) => maximum(l).max(maximum(r))

  def depth[A](tree: Tree[A]): Int = tree match
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + (depth(l).max(depth(r)))

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match
    case Leaf(v)      => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match
    case Leaf(v)      => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))

  def fsize[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(1 + _ + _)

  def fdepth[A](tree: Tree[A]): Int = fold(tree)(_ => 1)((l, r) => 1 + (l.max(r)))

  def fmaximum[A](tree: Tree[A])(implicit o: A => Ordered[A]): A = fold(tree)(a => a)(_ max _)

  def fmap[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
