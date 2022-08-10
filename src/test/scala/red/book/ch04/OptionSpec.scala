package red.book.ch04

import munit.Assertions.*
import munit.FunSuite
import red.book.ch04.Option.*

class OptionSpec extends FunSuite with OptionSpecContext:

  test("map transforms value if Some is defined") {
    assertEquals(Some(5).map(_ * 2), Some(10))
    assertEquals((None: Option[Int]).map(_ * 2), None)
  }

  test("flatMap applys f, which may fail, to the Option if not None") {
    assertEquals(Some(5).flatMap(v => Some(v * 2)), Some(10))
    assertEquals(Some(5).flatMap(_ => None), None)
    assertEquals((None: Option[Int]).flatMap(v => Some(v * 2)), None)
    assertEquals((None: Option[Int]).flatMap(_ => None), None)
  }

  test("getOrElse returns some value, if any, or else a default value") {
    assertEquals(Some(5).getOrElse(3), 5)
    assertEquals(None.getOrElse(3), 3)
  }

  test("orElse returns itself if a value is defined, or else the result of a default expression") {
    assertEquals(Some(5).orElse(Some(3)), Some(5))
    assertEquals(None.orElse(Some(3)), Some(3))
  }

  test("filter returns Some value if predicate is true, and None otherwise") {
    assertEquals(Some(5).filter(_ > 1), Some(5))
    assertEquals(Some(5).filter(_ % 2 == 0), None)
    assertEquals(Option.none[Int].filter(_ => true), None)
  }

  test("sequence returns an option list from a list of option") {
    assertEquals(sequence(List(Some(1), Some(2), Some(3), Some(4))), Some(List(1, 2, 3, 4)))
    assertEquals(sequence(List(Some(1), Some(3), None, Some(4))), None)
  }

  test("traverse maps over a list using a function that might fail") {
    def parseInts(ints: List[String]): Option[List[Int]] = traverse(ints)(i => Try(i.toInt))

    assertEquals(parseInts(List("1", "2", "3", "4")), Some(List(1, 2, 3, 4)))
    assertEquals(parseInts(List("ra")), None)
  }

  test("sequence_via_traverse returns an option list from a list of option") {
    assertEquals(sequence_via_traverse(List(Some(1), Some(2), Some(3), Some(4))), Some(List(1, 2, 3, 4)))
    assertEquals(sequence_via_traverse(List(Some(1), Some(3), None, Some(4))), None)
  }

trait OptionSpecContext:

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch case _: Exception => None
