package red.book.ch13

import red.book.ch11.Monad

object IO extends Monad[IO]:

  override def unit[A](a: => A): IO[A] = new IO[A]:
    def run: A = a
  override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)
  def apply[A](a: => A): IO[A] = unit(a)

  def forever[A, B](a: IO[A]): IO[B] =
    lazy val t: IO[B] = a.flatMap(_ => t)
    t

trait IO[A]:
  self =>
  def run: A
  def map[B](f: A => B): IO[B] = new IO[B]:
    def run: B = f(self.run)
  def flatMap[B](f: A => IO[B]): IO[B] = new IO[B]:
    def run: B = f(self.run).run
