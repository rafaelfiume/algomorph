package red.book.ch12

import red.book.ch10.Monoid
import red.book.ch11.Functor

import scala.collection.immutable.List.empty

trait Applicative[F[_]] extends Functor[F] {

  // primitive combinators
  def map2[A, B, C](a: F[A], b: F[B])(f: (A, B) => C): F[C]

  def unit[A](a: => A): F[A]

  // derived combinators

  def map[A, B](a: F[A])(f: A => B): F[B] = map2(a, unit(()))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = as.foldRight(unit(empty[B])) { (x, bs) =>
    map2(f(x), bs)(_ :: _)
  }

  // 12.1
  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(identity)
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

  // 12.12
  def sequenceMap[K, V](m: Map[K, F[V]]): F[Map[K, V]] = m.foldLeft(unit(Map.empty[K, V])) { case (acc, (k, fv)) =>
    map2(fv, acc)((v, m) => m + (k -> v))
  }

  // note 5
  def map2BasedOnProductAndMap[A, B, C](a: F[A], b: F[B])(f: (A, B) => C): F[C] = map(product(a, b))(f.tupled)

  // 12.8  // using "type lambda", equivalent to:     Applicative[(F[?], G[?])
  def product[G[_]](G: Applicative[G]): Applicative[({type f[a] = (F[a], G[a])})#f] = new Applicative[({type f[a] = (F[a], G[a])})#f] {
    override def map2[A, B, C](a: (F[A], G[A]), b: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) = {
      (Applicative.this.map2(a._1, b._1)(f), G.map2(a._2, b._2)(f))
    }
    override def unit[A](a: => A): (F[A], G[A]) = (Applicative.this.unit(a), G.unit(a))
  }

  // 12.9
  def compose[G[_]](G: Applicative[G]) = {
    val F = this
    new Applicative[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))
      override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        F.map2(fga, fgb)((ga, gb) => G.map2(ga, gb)(f))
    }
  }

}

// 12.2
trait Apply[F[_]] extends Functor[F] { // alternate impl of Applicative, with unit and apply as primitives

  // implement apply in terms of map2 and unit
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)(_ (_))

  def unit[A](a: A): F[A]

  // implement map2 and map in terms of unit and apply
  def map[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val fBtoC: F[B => C] = map(fa)(f.curried)
    apply(fBtoC)(fb)
  }

  // 12.3 based on unit and apply
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  // 12.8                               Applicative[(F[?], G[?])
  def product[G[_]](G: Apply[G]): Apply[({type f[x] = (F[x], G[x])})#f] = {
    val self = this

    new Apply[({type f[x] = (F[x], G[x])})#f] {
      override def unit[A](a: A): (F[A], G[A]) = (self.unit(a), G.unit(a))

      override def apply[A, B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])): (F[B], G[B]) =
        (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))
    }
  }

  // 12.9                                    Apply[G[F[?]]
  def compose[G[_]](G: Apply[G]) = {
    val F = this
    new Apply[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: A): F[G[A]] = F.unit(G.unit(a))
      override def apply[A, B](fg: F[G[A => B]])(fga: F[G[A]]): F[G[B]] = {
//        F.map2(fg, fga)((f, ga) => G.map2(f, ga)(_(_)))
        F.map2(fg, fga)((f, fa) => G.apply(f)(fa))
      }
    }
  }
}

sealed trait Validated[+E, +A]
case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validated[E, Nothing]
case class Success[A](v: A) extends Validated[Nothing, A]

// 12.6
object ApplicativeInstances {

  def validatedApplicative[E]: Applicative[({type f[x] = Validated[E, x]})#f] = new Applicative[({type f[x] = Validated[E, x]})#f] {
    override def map2[A, B, C](a: Validated[E, A], b: Validated[E, B])(f: (A, B) => C): Validated[E, C] = (a, b) match {
      case (Success(a), Success(b)) => Success(f(a, b))
      case (Success(_), f@Failure(_, _)) => f
      case (f@Failure(_, _), Success(_)) => f
      case (Failure(e1, t1), Failure(e2, t2)) => Failure(e1, (t1 :+ e2) ++ t2)
    }

    override def unit[A](a: => A): Validated[E, A] = Success(a)
  }

  def optionApplicative: Applicative[Option] = new Applicative[Option] {
    override def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
      case (Some(va), Some(vb)) => Some(f(va, vb))
      case _ => None
    }

    override def unit[A](a: => A): Option[A] = Some(a)
  }

  type Id[A] = A
  def idApplicative: Applicative[Id] = new Applicative[Id] {
    override def map2[A, B, C](a: A, b: B)(f: (A, B) => C): C = f(a, b)
    override def unit[A](a: => A): A = a
  }

  // turning a Monoid into an Applicative
  type Const[M, B] = M                                                                                     // Applicative[Const[M, ?]]
  implicit def monoidApplicative[M](M: Monoid[M]) = new Applicative[({type f[x] = Const[M, x]})#f] {
    override def map2[A, B, C](m1: M, m2: M)(f: (A, B) => C): M = M.op(m1, m2)
    override def unit[A](a: => A): M = M.zero
  }

  // TODO 12.7
  // TODO 12.10

}