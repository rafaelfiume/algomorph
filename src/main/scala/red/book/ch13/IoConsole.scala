package red.book.ch13

import red.book.ch07.Nonblocking.Par
import red.book.ch11.Monad
import red.book.ch13.Console.{ printLn, readLn }
import red.book.ch13.IoConsole.Println
import red.book.ch13.Translate.~>

import scala.io.StdIn

object IoConsole {

  def SimplePrintln(msg: String): IO[Unit] = IO { println(msg) }

  def Println(msg: String): TailRec[Unit] = TSuspend(() => println(msg))
}

object RunThisThing extends App {

  val f: Int => TailRec[Int] = x => TReturn(x)
  val g = List.fill(1000000)(f).foldLeft(f) {
    (a, b) => x => TSuspend(() => ()).flatMap(_ => a(x).flatMap(b))
  }
  println(s"result is ${TailRec.run(g(2))}")

//  IO.forever(SimplePrintln("print this message")).run // this will blow up the stack trace when called

//  TailRec.run(forever(Println("print this message"))) // this will println forever

  for {
    _ <- Println("bla")
  } yield ()
}

///////////// Freeeeeee ////////////

sealed trait Console[A] {
  def toPar: Par[A]     // interpret Console[A] as Par[A]
  def toThunk: () => A  // interpret Console[A] as Funtion0[A]
}

case object ReadLine extends Console[Option[String]] {
  override def toPar: Par[Option[String]] = Par.lazyUnit(run)
  override def toThunk: () => Option[String] = () => run

  private def run: Option[String] =
    try Some(StdIn.readLine())
    catch { case _: Exception => None }
}

case class PrintLine(msg: String) extends Console[Unit] {
  override def toPar: Par[Unit] = Par.lazyUnit(println(msg))
  override def toThunk: () => Unit = () => println(msg)
}

object Console {
  type ConsoleIO[A] = Free[Console, A]

  def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)
  def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))

  private val consoleToFuction0 = new (Console ~> Function0) {
    override def apply[A](fa: Console[A]): () => A = fa.toThunk
  }
  private val consoleToPar = new (Console ~> Par) {
    override def apply[A](fa: Console[A]): Par[A] = fa.toPar
  }

  private implicit val function0Monad: Monad[Function0] = new Monad[Function0] {
    override def unit[A](a: => A): () => A = () => a
    override def flatMap[A, B](fa: () => A)(f: A => () => B): () => B = f(fa())
  }
  private implicit val parMonad: Monad[Par] = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)
    override def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(fa)(f)
  }

  def runConsoleFunction0[A](free: Free[Console, A]): () => A = Free.runFree(free)(consoleToFuction0)
  def runConsolePar[A](free: Free[Console, A]): Par[A] = Free.runFree(free)(consoleToPar)
}

case class ConsoleReader[A](run: String => A) {
  def map[B](f: A => B): ConsoleReader[B] = ???
  def flatMap[B](f: A => ConsoleReader[B]): ConsoleReader[B] = ???
}

object RunThisThingPartll extends App {

  val f1: Free[Console, Option[String]] = for {
    _ <- printLn("Interacting with the console.... That's all I can do")
    l <- readLn
  } yield l

}
