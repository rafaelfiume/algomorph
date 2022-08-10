package red.book.ch12

import red.book.ch03.Tree.unit
import red.book.ch03.{Branch, Leaf, Tree}
import red.book.ch06.State
import red.book.ch06.State.{get, set}
import red.book.ch10.{Foldable, Monoid}
import red.book.ch11.{Functor, Monad, Monads}
import red.book.ch12.ApplicativeInstances.{Const, Id, idApplicative, monoidApplicative}
import red.book.ch12.Traverses.{listTraverse, optionTraverse, treeTraverse}

import scala.Option.empty

trait Traverse[F[_]] extends Functor[F] with Foldable[F]:

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = sequence(map(fa)(f))

  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(ga => ga)

  // 12.14 // traverse is a generalization of map - a traversable functor
  override def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[Id, A, B](fa)(f)(idApplicative)

  override def foldMap[A, M](fa: F[A])(f: A => M)(implicit m: Monoid[M]): M =
    traverse[({ type f[x] = Const[M, x] })#f, A, M](fa)(f)(monoidApplicative(m))

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({ type f[x] = State[S, x] })#f, A, B](fa)(f)(Monads.stateMonad)

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    traverseS(fa) { a =>
      for
        i <- get[Int]
        _ <- set(i + 1)
      yield (a, i)
    }.run(0)._1

  def zipWithIndex2[A](fa: F[A]): F[(A, Int)] = {
    mapAccum(fa, 0)((a, i) => ((a, i), i + 1))
  }._1

  override def toList[A](fa: F[A]): List[A] =
    traverseS(fa) { a =>
      for
        as <- get[List[A]]
        _ <- set(a :: as)
      yield ()
    }.run(List.empty[A])._2.reverse

  def toList2[A](fa: F[A]): List[A] = {
    mapAccum(fa, List.empty[A])((a, as) => ((), a :: as))
  }._2.reverse
  def toList3[A](fa: F[A]): List[A] =
    foldLeft(fa)(List.empty[A])((as, a) => a :: as).reverse

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa) { a =>
      for
        s1 <- get[S]
        (b, s2) = f(a, s1)
        _ <- set(s2)
      yield b
    }.run(s)

  // 12.16
  def reverse[A](fa: F[A]): F[A] = mapAccum(fa, toList2(fa).reverse)((_, as) => (as.head, as.tail))._1

  // 12.17
  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B = mapAccum(fa, z)((a, acc) => ((), f(acc, a)))._2

  // 12.18
  def fuse[G[_], H[_], A, B](
    fa: F[A]
  )(f: A => G[B], g: A => H[B])(implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[({ type f[x] = (G[x], H[x]) })#f, A, B](fa)(a => f(a) -> g(a))(G.product(H))

  // 12.19
  def compose[G[_]](implicit G: Traverse[G]): Traverse[({ type f[x] = F[G[x]] })#f] =
    val F = this
    new Traverse[({ type f[x] = F[G[x]] })#f]:
      override def traverse[M[_]: Applicative, A, B](fa: F[G[A]])(f: A => M[B]): M[F[G[B]]] =
        F.traverse(fa)(ga => G.traverse(ga)(f))

object Traverses:

  // 12.13
  val listTraverse: Traverse[List] = new Traverse[List]:
    override def traverse[G[_], A, B](as: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      as.foldRight(G.unit(List.empty[B])) { (a, glbs) =>
        G.map2(f(a), glbs)(_ :: _)
      }

  // 12.13
  val optionTraverse: Traverse[Option] = new Traverse[Option]:
    override def traverse[G[_], A, B](oa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      oa.fold(G.unit(empty[B])) { a =>
        G.map(f(a))(Some(_))
      }

  // 12.13
  val treeTraverse: Traverse[Tree] = new Traverse[Tree]:
    // book solution uses #head and #tail methods from Tree
    override def traverse[G[_], A, B](ta: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      Tree.fold(ta)(a => G.map(f(a))(unit)) { (l, r) =>
        G.map2(l, r)(Branch(_, _))
      }

  // 12.20
  def composeM[F[_], G[_]](F: Monad[F], G: Monad[G], T: Traverse[G]) = new Monad[({ type f[x] = F[G[x]] })#f]:
    override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a)) // ga => F[G[B]]
    override def flatMap[A, B](fga: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
      F.flatMap(fga)(ga => F.map(T.traverse(ga)(f)(F))(ggb => G.join(ggb)))

object Test extends App:

  implicit val applicativeOption: Applicative[Option] = ApplicativeInstances.optionApplicative
  val result = treeTraverse.traverse(aTree)(Option(_))
  println(result)

  def aTree: Tree[Int] = Branch(
    left = Branch(left = Leaf(1), right = Branch(left = Leaf(2), right = Leaf(3))),
    right = Branch(left = Leaf(7), right = Leaf(8))
  )

  // map
  println {
    listTraverse.map(List(1, 2, 3, 4))(_.toString) // List("1","2","3","4")
  }
  println {
    listTraverse.map(List(1, 2, 3, 4))(_ - 1) // List(0,1,2,3)
  }
  println {
    optionTraverse.map(Some(5))(_ * 2) // Some(10)
  }
  println {
    optionTraverse.map(empty[Int])(_ * 2) // None
  }

  // zipWithIndex
  println {
    listTraverse.zipWithIndex(List(23, 62, -3, 14, 25))
  }
  println {
    optionTraverse.zipWithIndex(Some(5))
  }
  println {
    optionTraverse.zipWithIndex(None)
  }

  // toList
  println {
    treeTraverse.toList3(aTree)
  }

//  // reverse
//  println {
//    treeTraverse.reverse(aTree)
//  }
//  println {
//    listTraverse.reverse(List(1, 2, 3, 4))
//  }
//
//  // foldLeft
//  println {
//    treeTraverse.foldLeft(aTree)(0)(_ + _)
//  }
//  println {
//    listTraverse.foldLeft(List(1, 2, 3, 4))(1)(_ * _)
//  }
