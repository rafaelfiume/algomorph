package red.book.ch10

import red.book.ch03.{ Branch, Leaf, Tree }
import red.book.ch10.Monoids.endoMonoid

import scala.collection.immutable.List.empty

trait Foldable[F[_]] {

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = {
    val ff: A => B => B = f.curried

    foldLeft(as)((b: B) => b)((g, a) => ff(a) andThen g)(z)
  }

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = {
    def ff(f: (B, A) => B): A => B => B = a => b => f(b, a)

    foldMap[A, B => B](as)(ff(f))(endoMonoid)(z)
  }

  def foldMap[A, M](as: F[A])(f: A => M)(implicit m: Monoid[M]): M = foldRight(as)(m.zero)((a, b) => m.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldMap(as)(a => a)(m)

  def toList[A](as: F[A]): List[A] = foldRight(as)(empty[A])(_ :: _)

}

object Foldable {

  val foldableList = new Foldable[List] {

    override def foldMap[A, B](as: List[A])(f: A => B)(implicit m: Monoid[B]): B = as.foldRight(m.zero)((a, b) => m.op(f(a), b))

    // It is entirely possible to define Foldable[List] in term of the following implementations =>

    //    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as match {
    //      case Nil => z
    //      case h :: t => foldLeft(t)(f(z, h))(f)
    //    }

    //    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as match {
    //      case Nil => z
    //      case h :: t => f(h, foldRight(t)(z)(f))
    //    }

  }

  val foldableTree = new Foldable[Tree] {
    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
        case Leaf(v) => f(v, z)
        case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
      }

    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
      case Leaf(v) => f(z, v)
      case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }

    // This implementation doesn't use m.zero since there is no empty tree
    // This is foldable without needing zero, thus it's a semigroup
    override def foldMap[A, B](as: Tree[A])(f: A => B)(implicit m: Monoid[B]): B = as match {
      case Leaf(v) => f(v)
      case Branch(l, r) => m.op(foldMap(l)(f)(m), foldMap(r)(f)(m))
    }
  }

  val foldableOption = new Foldable[Option] {
    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
      case None => z
      case Some(v) => f(v, z)
    }

    override def foldMap[A, B](as: Option[A])(f: A => B)(implicit m: Monoid[B]): B = as match {
      case None => m.zero
      case Some(v) => f(v)
    }
  }

}

object JustRun extends App {

  import Foldable._

  println {
    foldableList.foldRight(List(1,2,3))(0)(_ + _)
  }

  println {
    foldableList.foldRight(List(1,2,3))(empty[Int])(_ :: _)
  }

  println {
    foldableTree.foldRight(aTree)(0)(_ + _)
  }
  println {
    "toList >>>> " + foldableTree.toList(aTree)
  }

  println {
    foldableOption.foldRight(Some(5))(3)(_ + _)
  }
  println {
    "toList >>>>> " + foldableOption.toList(Some(5))
  }

  println {
    "toList >>>>>> " + foldableOption.toList(None)
  }


  def aTree: Tree[Int] = Branch(
    left = Branch(
      left = Leaf(1),
      right = Branch(left = Leaf(2), right = Leaf(3))),
    right = Branch(
      left = Leaf(7),
      right = Leaf(8))
  )
}
