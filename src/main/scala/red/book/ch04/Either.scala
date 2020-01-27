package red.book.ch04

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(v) => Right(f(v))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case l@Left(_) => l
    case Right(v) => f(v)
  }

  def orElse[EE >: E, AA >: A](default: Either[EE, AA]): Either[EE, AA] = this match {
    case Left(_) => default
    case r@Right(_) => r
  }

  def map2[EE >: E, B, C](y: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a <- this
      b <- y
    } yield f(a, b)

}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def sequence[E, A](list: List[Either[E, A]]): Either[E, List[A]] = list.foldRight(right[E, List[A]](List.empty[A])) {
    (e, acc) => e.map2(acc)(_ :: _)
  }

  def sequence_via_traverse[E, A](list: List[Either[E, A]]): Either[E, List[A]] = traverse(list)(x => x)

  def traverse[E, A, B](list: List[A])(f: A => Either[E, B]): Either[E, List[B]] = list.foldRight[Either[E, List[B]]](Right(Nil)) {
    (e, acc) => f(e).map2(acc)(_ :: _)
  }

  def right[E, A](value: A): Either[E, A] = Right(value)
  def left[A](value: Any): Either[Any, A] = Left(value)
}
