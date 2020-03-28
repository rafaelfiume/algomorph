package red.book.ch10

import java.lang.Integer.parseInt

import red.book.ch10.Monoids._

trait Monoid[A] {

  def op(a1: A, a2: A): A
  def zero: A

}

object Monoids {

  val intAddition = new Monoid[Int] {
    override def op(a1: Int, a2: Int) = a1 + a2
    override def zero = 0
  }
  val intMultiplication = new Monoid[Int] {
    override def op(a1: Int, a2: Int) = a1 * a2
    override def zero = 1
  }
  val booleanOr = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean) = a1 || a2
    override def zero = false
  }
  val booleanAnd = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean) = a1 && a2
    override def zero = true
  }
  // implementation by book authors is completely different...
  def optionMonoid[A](monoid: Monoid[A]) = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]) = (a1, a2) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(a1), Some(a2)) => Some(monoid.op(a1, a2))
    }
    override def zero = Option(monoid.zero)
  }
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A) = a1 andThen a2
    override def zero: A => A = a => a
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def op(a1: A, a2: A): A = m.op(a2, a1)
    override def zero: A = m.zero
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero)((b, a) => m.op(f(a), b))

  def fancyFoldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    foldMap(as, endoMonoid[B])(f.curried)(z)
  }

  def fancyFoldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)
  }

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (as.isEmpty) return m.zero // does this make sense? it does.
    if (as.size == 1) return f(as(0))

    val (seq1, seq2) = as.splitAt(as.size / 2)
    m.op(foldMapV(seq1, m)(f), foldMapV(seq2, m)(f))
  }

  /////////// Ex 10.9 ////////////////////////////////////////////////////////////

  sealed trait Track
  case class TNumber(num: Int) extends Track
  case class Interval(min: Int, max: Int) extends Track
  case class True() extends Track
  case class False() extends Track

  // a more concise/elegant version implemented by the book
  val monOrd = new Monoid[Track] {
    override def op(a1: Track, a2: Track): Track = (a1, a2) match {
      case (True(), track) => track
      case (track, True()) => track
      case (False(), _)   => False()
      case (_, False())   => False()
      case (TNumber(n1), TNumber(n2)) => if (n1 <= n2) Interval(n1, n2) else False()
      case (Interval(n1, n2), TNumber(n3)) => if (n2 <= n3) Interval(n1, n3) else False()
      case (TNumber(n1), Interval(n2, n3)) => if (n1 <= n2) Interval(n1, n3) else False()
      case (Interval(n1, _), Interval(_, n4)) => Interval(n1, n4) // don't really care
    }
    override def zero: Track = True()
  }
  def ordered(as: IndexedSeq[Int]): Boolean = {
      foldMapV(as, monOrd)(i => TNumber(i)) match {
        case False() => false
        case _ => true
      }
  }

  // ordered - with fold

  case class TrackInterval(min: Int, max: Int, ordered: Boolean)

  def orderedFF(as: IndexedSeq[Int]): Boolean = {
    val h = as.head
    val t = as.tail
    t.tail.foldLeft(TrackInterval(h, t.head, true)) {
      case (TrackInterval(min, max, true), next) if max - min >= 0 => TrackInterval(max, next, true)
      case (b, next) => TrackInterval(b.max, next, false)
    }.ordered
  }

  val monOrdBook = new Monoid[Option[(Int, Int, Boolean)]] {
    override def op(a1: Option[(Int, Int, Boolean)], a2: Option[(Int, Int, Boolean)]) = (a1, a2) match {
      case (Some((x1, y1, b1)), Some((x2, y2, b2))) => Some((x1 min x2, y1 max y2, b1 && b2 && y1 <= x2))
      case (a1, None) => a1
      case (None, a2) => a2
    }

    override def zero = None
  }

  def orderedBook(as: IndexedSeq[Int]): Boolean = {
    foldMapV(as, monOrdBook)(i => Some((i, i, true))).map(_._3).getOrElse(true)
  }

  /////////////////////////////////////////////////////////////////////////////////////////


}

object RunThisStuff extends App {

  val (o1, o2) = (Option(1), Option(2))

  println("<<<<<< ordered with monoid >>>>>")
  println {
    ordered(Vector()) // true
  }
  println {
    ordered(Vector(1)) // true
  }
  println {
    ordered(Vector(2,2,2,2)) // true
  }
  println {
    ordered(Vector(-1, 0, 2, 5,7)) // true
  }
  println {
    ordered(Vector(1, 4, 6, 5, 7, 8)) // false It fails here
  }
  println {
    ordered(Vector(9, 0, 2, 5,7)) // false
  }
  println {
    ordered(Vector(-1, -10)) // false
  }

  println("<<<<<< ordered with folder >>>>>")
  println {
    orderedFF(Vector(-1, 0, 2, 5,7)) // true
  }
  println {
    orderedFF(Vector(9, 0, 2, 5,7)) // false
  }

  println("<<<<<< ordered with monoid - book version >>>>>")
  println {
    orderedBook(Vector()) // true
  }
  println {
    orderedBook(Vector(1)) // true
  }
  println {
    orderedBook(Vector(2,2,2,2)) // true
  }
  println {
    orderedBook(Vector(-1, 0, 2, 5,7)) // true
  }
  println {
    orderedBook(Vector(1, 4, 6, 5, 7, 8)) // false
  }
  println {
    orderedBook(Vector(9, 0, 2, 5,7)) // false
  }
  println {
    orderedBook(Vector(-1, -10)) // false
  }

  println("<<<<<< others >>>>>")

  println {
    optionMonoid(intAddition).op(o1, o2)
  }

  println {
    foldMap(List("1", "2", "3"), intAddition)(parseInt)
  }

  println {
    fancyFoldLeft(List(1, 2, 3))(0)(_ + _)
  }

  println {
    foldMapV(Vector("2", "2", "3"), intMultiplication)(parseInt)
    foldMapV(Vector[String](), intMultiplication)(parseInt)
  }
}
