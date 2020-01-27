package red.book.ch10

object Composable {

  def productMonoid[A, B](implicit ma: Monoid[A], mb: Monoid[B]) = new Monoid[(A, B)] {
    override def op(x: (A, B), y: (A, B)): (A, B) = (ma.op(x._1, y._1), mb.op(x._2, y._2))
    override def zero: (A, B) = (ma.zero, mb.zero)
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    override def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] = (a1.keys ++ a2.keys).foldLeft(zero) { (m, k) =>
      m + (k -> V.op(a1.getOrElse(k, V.zero), a2.getOrElse(k, V.zero)))
    }
    override def zero: Map[K, V] = Map.empty
  }

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def op(f: A => B, g: A => B): A => B = a => B.op(f(a), g(a))
    override def zero: A => B = _ => B.zero
  }
}

object Bag {
  import Composable._
  import Monoids._

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = foldMapV(as, mapMergeMonoid[A, Int](intAddition))(a => Map(a -> 1))
}

object ComposeAndRun extends App {

  import Composable._
  import Monoids._

  val mm: Monoid[Map[String, Map[String, Int]]] = mapMergeMonoid(mapMergeMonoid(intAddition))

  val m1 = Map(
    "a" -> Map("x" -> 1, "y" -> 2, "z" -> 3),
    "b" -> Map("r" -> 11, "s" -> 22)
  )
  val m2 = Map(
    "a" -> Map("x" -> 2, "y" -> 1, "w" -> 3),
    "c" -> Map("oo" -> 44)
  )

  println {
    "mergeMapMonoid in action >>>> " + mm.op(m1, m2)
  }

  def fm = functionMonoid[Int, Int](intMultiplication)
  def sum2: Int => Int = a => a + 2

  println {
    "functionMonoid in action >>>>>> " + fm.op(sum2, sum2)(5)
  }

  println {
    "bag in action >>>>" + Bag.bag("nicolas einsfeld fiume")
  }
}