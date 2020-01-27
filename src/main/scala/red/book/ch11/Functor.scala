package red.book.ch11

trait Functor[F[_]] {

  def map[A, B](fa: F[A])(f: A => B): F[B]

  // a generic unzip that works for any functor!
  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = map(fab)(_._1) -> map(fab)(_._2)

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Right(fa) => map(fa)(Right(_))
    case Left(fb) => map(fb)(Left(_))
  }
}

object Functors {

  val listFunctor = new Functor[List] {
    override def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }
}
