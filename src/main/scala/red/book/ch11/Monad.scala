package red.book.ch11

import red.book.ch06.State
import red.book.ch12.Applicative

import scala.collection.immutable.List.empty

trait Monad[F[_]] extends Applicative[F] {

  /*
   * Minimal Set of Monad Combinators
   *
   * A) unit & flatMap
   * B) unit & compose
   * C) unit & map & join
   */

  def unit[A](a: => A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa){ a => map(fb) { b => f(a, b) } }
  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))

  override def sequence[A](as: List[F[A]]): F[List[A]] = traverse(as)(identity)

  override def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = as.foldRight(unit(empty[B])) { (a, acc) =>
    map2(f(a), acc)(_ :: _)
  }

  //  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = map(ma) { a => List.fill(n)(a) } // this fails for list
  override def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma)) // book

  def filterM[A](ls: List[A])(f: A => F[Boolean]): F[List[A]] = ls match {
    case Nil => unit(empty)
    case x :: xs => flatMap(f(x)) { p =>
      if (p) {
        map2(unit(x), filterM(xs)(f))(_ :: _)
     // map(filterM(xs)(f))(x :: _) // this is the solution from the authors, which makes the stack shorter
      } else {
        filterM(xs)(f)
      }
    }
  }

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

  // don't invoke this or there will be a stack overflow
  // we are implementing flatMap based on compose just for demonstrating that compose and unit could be the minimal set of a Monad combinator
  def mapBasedOnCompose[A, B](ma: F[A])(f: A => B): F[B] = {
    val r: Any => F[B] = compose(_ => ma, (a: A) => unit(f(a)))
    r(())
  }

  // see note above
  def flatMapBasedOnCompose[A, B](ma: F[A])(f: A => F[B]): F[B] = compose((_: Unit) => ma, f)(())

  // see note above
  def flatMapBasedOnJoinAndMap[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  // see also note above
  def composeBasedOnJoinAndMap[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => join(map(f(a))(g))

  // 11.12
  // this is the equivalent of flatten
  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(identity)

//   12.11 impossible Monad composition
//  def compose[G[_]](G: Monad[G]) = {
//    val F = this
//    new Monad[({type f[x] = F[G[x]]})#f] {
//      override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))
//
//      override def flatMap[A, B](fa: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
//        F.flatMap(fa) { ga => G.flatMap(ga)(f)}
//    }
//  }

}

// TODO 11.9

// TODO 11.10

// TODO 11.11

// TODO 11.14

/* 11.15
 *
 * Associative Law: (answer from the authors is very different)
 *
 * Par - It doesn't matter the order the parallel computation completes, the result will always be the same
 *
 * Parser - It doesn't matter the order of the elements being parsed, the parsed result will always be the same
 */

/* 11.16
 *
 * Identity Law: (answer from the authors is different)
 *
 * Gen - If you compose a generator with a gen that always give you the same element, you get the original gen as result.
 *
 * List - If you compose a list with an empty list (the id element for a list), you get the original list as result.
 */

object Monads {

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa flatMap f
  }

  val streamMonad = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream.continually(a)
    override def flatMap[A, B](fa: Stream[A])(f: A => Stream[B]): Stream[B] = fa flatMap f
  }

  val listMonad = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa flatMap f
  }

  // 11.17
  val idMonad = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)
    override def flatMap[A, B](id: Id[A])(f: A => Id[B]): Id[B] = id flatMap f
  }
  case class Id[A](value: A) {
    def map[B](f: A => B): Id[B] = Id(f(value))
    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
  }

  // 12.5
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }
  }

  // The challenge here is: Monad trait requires a type constructor with *one* type parameter, but State requires *two*
  // The solution is to fix one of the type parameters (conventionally, the one in the left side).
  // Follows a too specialized example of a State Monad implementation where the type of State is Int

  type IntState[A] = State[Int, A]
  def intStateMonad = new Monad[IntState] {
    override def unit[A](a: => A): IntState[A] = State.unit(a)
    override def flatMap[A, B](st: IntState[A])(f: A => IntState[B]): IntState[B] = st flatMap f
  }

  class StateMonad[S] {
    type StateS[A] = State[S, A]

    val stateMonad = new Monad[StateS] {
      override def unit[A](a: => A): State[S, A] = State.unit(a)
      override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] = st flatMap f
    }
  }

  def stateMonad[S] = new Monad[({type f[X] = State[S, X]})#f] {
    override def unit[A](a: => A): State[S, A] = State.unit(a)
    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] = st flatMap f
  }

}
