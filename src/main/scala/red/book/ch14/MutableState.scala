package red.book.ch14

import scala.collection.mutable
import scala.reflect.ClassTag

sealed trait ST[S, A]:
  self =>
  protected def run(s: S): (A, S)

  def map[B](f: A => B): ST[S, B] = new ST[S, B]:
    override protected def run(s1: S): (B, S) =
      val (a, s2) = self.run(s1)
      (f(a), s2)

  def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B]:
    override protected def run(s1: S): (B, S) =
      val (a, s2) = self.run(s1)
      f(a).run(s2)

object ST:
  def apply[S, A](a: => A): ST[S, A] =
    lazy val memo = a
    new ST[S, A]:
      override protected def run(s: S): (A, S) = (memo, s)

  def runST[A](st: RunnableST[A]): A = st.apply.run(())._1

//sealed trait STRef[S, A]:
//  protected var cell: A
//
//  def read: ST[S, A] = ST(cell)
//
//  def write(a: A): ST[S, Unit] = new ST[S, Unit]:
//    def run(s: S) =
////      cell = a
//      ((), s)
//
//object STRef:
//  def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(
//    new STRef[S, A]:
//      override protected var cell: A = a
//  )
// TODO error overriding variable cell in trait STRef of type A;
//[error]    |              variable cell of type A cannot override a mutable variable

trait RunnableST[A]:
  def apply[S]: ST[S, A]

sealed abstract class STArray[S, A]():
  protected def value: Array[A]

  def size: ST[S, Int] = ST(value.length)

  def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit]:
    override protected def run(s: S): (Unit, S) =
      value(i) = a
      ((), s)

  def read(i: Int): ST[S, A] = ST(value(i))

  def freeze: ST[S, List[A]] = ST(value.toList)

  // 14.1
  def fill(xs: Map[Int, A]): ST[S, Unit] = new ST[S, Unit]():
    override protected def run(s: S): (Unit, S) =
      xs.keys.foreach { key =>
        write(key, xs(key))
      }
      ((), s)

object STArray:
  def apply[S, A: ClassTag](sz: Int, v: A): ST[S, STArray[S, A]] = ST(
    new STArray[S, A]:
      override protected def value: Array[A] = Array.fill(sz)(v)
  )
  def fromList[S, A: ClassTag](xs: List[A]): ST[S, STArray[S, A]] = ST(
    new STArray[S, A]():
      override protected def value: Array[A] = xs.toArray
  )

sealed abstract class STMap[S, K, V]:
  protected val mMap: mutable.Map[K, V]

  def size: ST[S, Int] = ST(mMap.size)

  def get(key: K): ST[S, Option[V]] = ST(mMap.get(key))

  def write(key: K, value: V): ST[S, Unit] = new ST[S, Unit]:
    override protected def run(s: S): (Unit, S) =
      mMap.put(key, value)
      ((), s)

// TODO See compilation error above
//object RunThisThing extends App:
//
//  val unsafe = for // exposes STRef
//    r1 <- STRef[Nothing, Int](1)
//    r2 <- STRef[Nothing, Int](2)
//    x <- r1.read
//    y <- r2.read
//    _ <- r1.write(x + 1)
//    _ <- r2.write(y + 1)
//    a <- r1.read
//    b <- r2.read
//  yield (a, b)
//
//  val p = new RunnableST[(Int, Int)]:
//    override def apply[S]: ST[S, (Int, Int)] = for
//      r1 <- STRef(1)
//      r2 <- STRef(2)
//      x <- r1.read
//      y <- r2.read
//      _ <- r1.write(x + 1)
//      _ <- r2.write(y + 1)
//      a <- r1.read
//      b <- r2.read
//    yield (a, b)
//
//  println {
//    ST.runST(p)
//  }
