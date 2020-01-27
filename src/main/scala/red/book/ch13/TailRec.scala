package red.book.ch13

object TailRec {

  def forever[A,B](a: TailRec[A]): TailRec[B] = {
    lazy val t: TailRec[B] = a flatMap (_ => t)
    t
  }

  @annotation.tailrec
  def run[A](tailRec: TailRec[A]): A = tailRec match {
    case TReturn(a) => a
    case TSuspend(r) => r()
    case TFlatMap(x, f) => x match {
      case TReturn(a) => run(f(a))
      case TSuspend(r) => run(f(r()))
      case TFlatMap(y, g) =>
        run(y flatMap (a => g(a) flatMap f))
    }
  }
}

sealed trait TailRec[A] {

  def flatMap[B](f: A => TailRec[B]): TailRec[B] = TFlatMap(this, f)
  def map[B](f: A => B): TailRec[B] = flatMap(f andThen TReturn.apply)
}

// data constructors representing the different kind of flows we want the interpreter to support

// represents an action that has finished
case class TReturn[A](a: A) extends TailRec[A]

// we want to execute some effect to produce a result
case class TSuspend[A](resume: () => A) extends TailRec[A]

// when run see this, it should compute sub first before evaluate k
// this let us extend or continue a computation using the result of the first computation to produce a second computation
case class TFlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]
// the interpreter can process the subcomputation sub and then remember to call the continuation k on the result.
// then k will continue executing the program