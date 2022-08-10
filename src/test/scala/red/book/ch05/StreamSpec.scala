package red.book.ch05

import munit.Assertions.*
import munit.FunSuite
import red.book.ch04.*
import red.book.ch05.Stream.*

import scala.{None as _, Option as _, Some as _}

class StreamSpec extends FunSuite:

  test("toList forces evaluation and convert stream to list") {
    assertEquals(Stream(1, 2, 3, 4, 5).toList, List(1, 2, 3, 4, 5))
  }

  test("take first n elements of a stream") {
    assertEquals(Stream(1, 2, 3, 4, 5).take(3).toList, List(1, 2, 3))
  }

  test("drop the first n elements of a stream") {
    assertEquals(Stream(1, 2, 3, 4, 5).drop(3).toList, List(4, 5))
  }

  test("take while elements of a stream matches a predicate") {
    assertEquals(Stream(1, 2, 3, 4, 5).takeWhile(_ < 3).toList, List(1, 2))
  }

  test("take while elements of a stream matches a predicate (fold-based)") {
    assertEquals(Stream(1, 2, 3, 4, 5).takeWhile_w_fold(_ < 3).toList, List(1, 2))
  }

  test("forAll checks if all elements of a stream match a given predicate") {
    assert(Stream(1, 2, 3, 4, 5).forAll(_ > 0))
    assert(!Stream(1, 2, 3, 4, 5).forAll(_ % 2 == 0))
  }

  test("headOption returns first element or none if stream is empty") {
    assertEquals(Stream(1, 2).headOption, Some(1))
    assertEquals(Stream.empty.headOption, None)
  }

  test("map applies function f to each element of a stream") {
    assertEquals(Stream(1, 2, 3).map(_ % 2).toList, List(1, 0, 1))
    assertEquals(Stream.empty[Int].map(_ % 2).toList, Nil)
  }

  test("filter elements of a stream that match a given predicate") {
    assertEquals(Stream(1, 2, 3, 4, 5).filter(_ % 2 == 0).toList, List(2, 4))
    assertEquals(Stream.empty[Int].filter(_ < 0).toList, Nil)
  }

  test("append another stream in to the original one") {
    assertEquals(Stream(4, 6, 7, 8).append(Stream(9, 0, 6)).toList, List(4, 6, 7, 8, 9, 0, 6))
  }

  test("flatMap applies function f in to each element of a stream") {
    assertEquals(Stream(6, 7, 8, 9).flatMap(x => Stream(x, x)).toList, List(6, 6, 7, 7, 8, 8, 9, 9))
  }

  test("create an infinite stream") {
    assertEquals(constant("a").take(5).toList, List("a", "a", "a", "a", "a"))
  }

  test("create an infinite n, n+1, etc, stream of elements from n") {
    assertEquals(from(6).take(3).toList, List(6, 7, 8))
  }

  test("fibs create an infinite stream of Fibonacci numbers") {
    assertEquals(fibs().take(7).toList, List(0, 1, 1, 2, 3, 5, 8))
  }

  test("unfold a stream using an initial state and a function to generate the next state and value") {
    assertEquals(unfold(6)(s => Some(s -> (s + 1))).take(3).toList, List(6, 7, 8))
  }

  test("unfold a stream using an initial state and a function to generate the next state and value (map-based)") {
    assertEquals(unfoldViaMap(6)(s => Some(s -> (s + 1))).take(3).toList, List(6, 7, 8))
  }

  test("unfold a stream using an initial state and a function to generate the next state and value (foldRight-based)") {
    assertEquals(unfoldViaFoldRight(6)(s => Some(s -> (s + 1))).take(3).toList, List(6, 7, 8))
  }

  test("unfold a stream using an initial state and a function to generate the next state and value (fold-based)") {
    assertEquals(unfoldViaFold(6)(s => Some(s -> (s + 1))).take(3).toList, List(6, 7, 8))
  }

  test("create an infinite stream of Fibonacci numbers (unfold-based)") {
    assertEquals(fibs_u().take(7).toList, List(0, 1, 1, 2, 3, 5, 8))
  }

  test("create an infinite n, n+1, etc, stream of elements from n (unfold based)") {
    assertEquals(from_u(6).take(3).toList, List(6, 7, 8))
  }

  test("create an infinite stream (unfold-based)") {
    assertEquals(Stream.constant_u("a").take(5).toList, List("a", "a", "a", "a", "a"))
  }

  test("creates an infinite stream of 1's (unfold-based)") {
    assertEquals(Stream.ones_u().take(5).toList, List(1, 1, 1, 1, 1))
  }

  test("map_u applies function f to each element of a stream (unfold-based)") {
    assertEquals(Stream(1, 2, 3).map_u(_ % 2).toList, List(1, 0, 1))
    assertEquals(Stream.empty[Int].map_u(_ % 2).toList, Nil)
  }

  test("takes the first n elements of a stream (unfold-based)") {
    assertEquals(Stream(1, 2, 3, 4, 5).take_u(3).toList, List(1, 2, 3))
    assertEquals(Stream(1, 2, 3, 4, 5).take_u(1).toList, List(1))
    assertEquals(Empty.take_u(3).toList, Nil)
  }

  test("take while elements of a stream matches a predicate (unfold-based)") {
    assertEquals(Stream(1, 2, 3, 4, 5).takeWhile_u(_ < 3).toList, List(1, 2))
    assertEquals(Stream.empty[Int].takeWhile_u(_ < 3).toList, Nil)
  }

  test("zipWith accepts two streams and construct a new one by applying a function on corresponding elements") {
    assertEquals(Stream(1, 2, 3, 4, 5).zipWith(Stream(6, 7, 8, 9))(_ + _).toList, List(7, 9, 11, 13))
    assertEquals(Stream(1, 2, 3, 4, 5).zipWith(Stream.empty[Int])(_ + _).toList, Nil)
    assertEquals(Stream.empty[Int].zipWith(Stream(6, 7, 8, 9))(_ + _).toList, Nil)
  }

  test(
    "zip creates a new stream containing the corresponding pair of elements from each stream as long as both streams have elements"
  ) {
    assertEquals(Stream(1, 2, 3, 4, 5).zip(Stream(6, 7, 8, 9)).toList, List((1, 6), (2, 7), (3, 8), (4, 9)))
    assertEquals(Stream(1, 2).zip(Stream.empty).toList, Nil)
    assertEquals(Stream.empty.zip(Stream(6, 7)).toList, Nil)
  }

  test("zipWithAll does the same as zipWith but continues the traversal as long as either stream has elements)") {
    assertEquals(
      Stream(1, 2, 3, 4, 5).zipAllWith(Stream(6, 7, 8, 9))((_, _)).toList,
      List((Some(1), Some(6)), (Some(2), Some(7)), (Some(3), Some(8)), (Some(4), Some(9)), (Some(5), None))
    )
    assertEquals(
      Stream(1, 2).zipAllWith(Stream.empty)((_, _)).toList,
      List((Some(1), None), (Some(2), None))
    )
    assertEquals(
      Stream.empty.zipAllWith(Stream(6, 7))((_, _)).toList,
      List((None, Some(6)), (None, Some(7)))
    )
  }

  test("zipAll continues the traversal as long as either stream has elements") {
    assertEquals(Stream(1, 2, 3, 4, 5).zipAll(Stream(6, 7, 8, 9)).toList,
                 List((Some(1), Some(6)), (Some(2), Some(7)), (Some(3), Some(8)), (Some(4), Some(9)), (Some(5), None))
    )
    assertEquals(
      Stream(1, 2).zipAll(Stream.empty).toList,
      List((Some(1), None), (Some(2), None))
    )
    assertEquals(
      Stream.empty.zipAll(Stream(6, 7)).toList,
      List((None, Some(6)), (None, Some(7)))
    )
  }

  test("startsWith checks if a list starts with a subsequence") {
    assert(Stream(1, 2, 3, 4, 5).startsWith(Stream(1, 2, 3)))
    assert(Stream(1, 2, 3, 4, 5).startsWith(Empty))
    assert(!Stream(1, 2, 3, 4, 5).startsWith(Stream(2, 3)))
    assert(!Stream(1, 2, 3).startsWith(Stream(1, 2, 3, 4)))
    assert(!Empty.startsWith(Stream(1)))
  }

  test("tails returns the tail of a stream") {
    assertEquals(
      Stream(1, 2, 3).tails.map(_.toList).toList,
      List(List(1, 2, 3), List(2, 3), List(3), Nil)
    )
    assertEquals(
      Stream(3).tails.map(_.toList).toList,
      List(List(3), Nil)
    )
  }

  test("hasSubsequence checks if a Stream contains another Stream as subsequence") {
    assert(Stream(1, 2, 3).hasSubsequence(Stream(2, 3)))
    assert(!Stream(1, 2, 3).hasSubsequence(Stream(3, 4)))
  }

  test("scanRight returns a stream with all the intermediate results") {
    assertEquals(
      Stream(1, 2, 3, 4, 5, 6).take(3).scanRight(0)(_ + _).toList,
      List(0 + 1 + 2 + 3, 0 + 2 + 3, 0 + 3, 0)
    )
  }

  test("scanLeft returns a stream with all the intermediate results") {
    assertEquals(
      Stream(1, 2, 3, 4, 5, 6).take(3).scanLeft(0)(_ + _).toList,
      List(0, 0 + 1, 0 + 1 + 2, 0 + 1 + 2 + 3)
    )
  }
