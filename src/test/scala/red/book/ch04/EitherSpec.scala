package red.book.ch04

import red.book.ch04.Either.*

import scala.util.control.NonFatal
import munit.Assertions.*
import munit.FunSuite

class EitherSpec extends FunSuite with EitherSpecContext:

  test("map applies function f on the right side of an element") {
    assertEquals(Right(2).map(_ * 3), Right(6))
    assertEquals(left[String, Int]("error").map(_ * 3), Left("error"))
  }

  test("orEls returns right value, or (in case of left), return the default  provided") {
    assertEquals(Right(2).orElse(Right(5)), Right(2))
    assertEquals(left("error").orElse(Right(5)), Right(5))
  }

  test("flatMap applies function f (which return an Either) on the right side of the element") {
    assertEquals(
      Right(2).flatMap { x => Right(x * 3) },
      Right(6)
    )
    assertEquals(
      Right(2).flatMap { _ => Left("error") },
      Left("error")
    )
    assertEquals(
      Either.left("error").flatMap { _ => Right("success") },
      Left("error")
    )
  }

  test("map2 lifts a function so it can be used with Either instances") {
    def agg(x: Int, y: Int) = x + y
    assertEquals(
      parseInt("2").map2(parseInt("4"))(agg),
      Right(6)
    )
    assertEquals(
      parseInt("bla").map2(parseInt("4"))(agg),
      left("For input string: \"bla\"")
    )
    assertEquals(
      parseInt("2").map2(parseInt("bla"))(agg),
      left("For input string: \"bla\"")
    )
  }

  test("sequence returns an either list with the first error it finds (if there is one) from a list of either") {
    assertEquals(
      sequence(List(Right(2), Right(3), Right(4), Right(5))),
      Right(List(2, 3, 4, 5))
    )
    assertEquals(
      sequence(List(Right(2), Left("error"), Left("bla"))),
      Left("error")
    )
  }

  test("sequence returns an either list with the first error it finds (if there is one) from a list of either (traverse-based)") {
    assertEquals(
      sequence_via_traverse(List(Right(2), Right(3), Right(4), Right(5))),
      Right(List(2, 3, 4, 5))
    )
    assertEquals(
      sequence_via_traverse(List(Right(2), Left("error"), Left("bla"))),
      Left("error")
    )
  }

  test("traverse over a list with a function that might fail") {
    assertEquals(traverse(List("1", "2", "3"))(parseInt), Right(List(1, 2, 3)))
    assertEquals(traverse(List("a", "f", "f"))(parseInt), Left("For input string: \"a\""))
  }

trait EitherSpecContext:

  def parseInt(s: String): Either[String, Int] = Try(s.toInt)

  def Try[A](a: => A) =
    try Right(a)
    catch case NonFatal(e) => Left(e.getMessage)
