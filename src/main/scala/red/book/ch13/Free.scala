package red.book.ch13

import red.book.ch07.Par.Par
import red.book.ch11.Monad
import red.book.ch13.Translate.~>

/*
 * The Return and FlatMap constructors witness that this data type is a monad for any choice of F,
 * and since they're exactly the operations required to generate a monad, we say that it's a free monad
 * (F doesn't need to have any monadic structure of its own).
 */

sealed trait Free[F[_], A] {

  // 13.1
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)
  def map[B](f: A => B): Free[F, B] = flatMap(f andThen (Return(_)))
}

case class Return[F[_], A](a: A) extends Free[F, A]
case class Suspend[F[_], A](fa: F[A]) extends Free[F, A]
case class FlatMap[F[_], A, B](fa: Free[F, A], k: A => Free[F, B]) extends Free[F, B]

object Free {

  // 13.1 // using "type lambda"
  def freeMonad[F[_]]: Monad[({type f[a] = Free[F, a]})#f] = new Monad[({type f[a] = Free[F, a]})#f] {
    override def unit[A](a: => A): Free[F, A] = Return(a)
    override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = fa flatMap f
  }

  // 13.2
  @annotation.tailrec
  def runTrampoline[A](free: Free[Function0, A]): A = free match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => runTrampoline(f(a))
      case Suspend(r) => runTrampoline(f(r()))
      case FlatMap(y, g) => runTrampoline(y flatMap(a => g(a) flatMap f))
    }
  }

  // 13.3
  def run[F[_], A](free: Free[F, A])(implicit F: Monad[F]): F[A] = step(free) match {
    case Return(a) => F.unit(a)
    case Suspend(r) => r
    case FlatMap(Suspend(r), f) => F.flatMap(r)(a => run(f(a)))
    case _ => sys.error("`step` eliminates these cases")
  }
  @annotation.tailrec
  private def step[F[_], A](free: Free[F, A]): Free[F, A] = free match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(a), f) => step(f(a))
    case _ => free
  }

  def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(implicit G: Monad[G]): G[A] = step(free) match {
    case Return(a) => G.unit(a)
    case Suspend(r) => t(r)
    case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
    case _ => sys.error("`step` eliminates these cases")
  }

  // TODO 13.4 eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee

  // TODO 13.5 ?????????????

}

trait Translate[F[_], G[_]] {
  def apply[A](fa: F[A]): G[A]
}

object Translate {
  type ~>[F[_], G[_]] = Translate[F, G]
}

object specialized {

  type TailRec[A] = Free[Function0, A]
  type Async[A] = Free[Par, A]
}
