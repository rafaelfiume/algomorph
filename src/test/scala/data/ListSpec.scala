package data

import munit.Assertions.*
import munit.FunSuite
import data.List.*

class ListSpec extends FunSuite:

  test("returns the size of a list"):
    assertEquals(size(Nil), 0)
    assertEquals(size(List(1, 2, 3, 4)), 4)
    assertEquals(size(List("a", "b")), 2)

  test("init returns all but the last element of the list"):
    assertEquals(init(List(1, 2, 3, 4)), List(1, 2, 3))
    assertEquals(init(List(1)), Nil)

  test("tail removes the first element of the list"):
    assertEquals(tail(List(1, 2, 3, 4)), List(2, 3, 4))
    assertEquals(tail(Nil), Nil)
    assertEquals(tail(List(1)), Nil)

  test("setHead replaces the first element of a list"):
    assertEquals(setHead(List(1, 2, 3, 4), 9), List(9, 2, 3, 4))
    assertEquals(setHead(List(1), 9), List(9))
    assertEquals(setHead(Nil, 9), Nil)

  test("appends one list to another") {
    assertEquals(append(List(1, 2, 3), List(4, 5, 6)), List(1, 2, 3, 4, 5, 6))
    assertEquals(append(Nil, List(1)), List(1))
  }

  test("concat flats the list"):
    assertEquals(concat(List(List(1, 2), List(3, 4), List(5, 6, 7))), List(1, 2, 3, 4, 5, 6, 7))
    assertEquals(concat(List(Nil, List(3, 4))), List(3, 4))
    assertEquals(concat(Nil), Nil)

  test("reverses the list"):
    assertEquals(reverse(List(1, 2, 3, 4)), List(4, 3, 2, 1))
    assertEquals(reverse(List(1)), List(1))
    assertEquals(reverse(Nil), Nil)

  test("drops the nth element of a list") {
    assert(drop(List(1, 2, 3, 4), 2) == List(3, 4))
    assert(drop(Nil, 1) == Nil)
    assert(drop(List(1, 2, 3, 4), 0) == List(1, 2, 3, 4))
  }

  test("drops elements of a list while they match predicate") {
    assert(dropWhile(List(-1, -2, 0, 1, 2, 3, 4), (x: Int) => x < 0) == List(0, 1, 2, 3, 4))
    assert(dropWhile(List(1, 2, 3, 4), (x: Int) => x > 0) == Nil)
    assert(dropWhile(Nil, (x: Int) => x > 0) == Nil)
    assert(dropWhile(List(1, 2, 3, 4), (x: Int) => x < 0) == List(1, 2, 3, 4))
  }

  test("foldLeft is left-associative"):
      // (((0 - 1) - 2) - 3) = -6
      val input = List(1, 2, 3)
      val result = foldLeft(input, 0)(_ - _)
      assertEquals(result, -6)

  test("foldRight reconstructs the original list"):
    val input = List(1, 2, 3, 4)
    val result = foldRight(input, List.empty[Int])(Cons(_, _))
    assertEquals(result, input)

  test("foldRight is right-associative (ops applied in reverse order)"):
    // (1 - (2 - (3 - 0))) = 2
    val input = List(1, 2, 3)
    val result = foldRight(input, 0)(_ - _)
    assertEquals(result, 2)

  test("map generalizes each element in a list while preserving its structure") {
    assertEquals(
      map(List(1, 2, 3, 4))(_.toString),
      List("1", "2", "3", "4")
    )
    assertEquals(
      map(List(1, 2, 3, 4))(_ - 1),
      List(0, 1, 2, 3)
    )
  }

  test("filter removes all elements that don't satisfy a predicate") {
    assertEquals(
      filter(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))(_ % 2 == 0),
      List(0, 2, 4, 6, 8)
    )
  }

  test("flatMap works like a map, but take a function that returns a list, and append that list in the final result") {
    assertEquals(
      flatMap(List(1, 2, 3, 4))(e => List(e, e)),
      List(1, 1, 2, 2, 3, 3, 4, 4)
    )
  }

  test("filterWithFlatMap removes all elements that don't satisfy a predicate") {
    assertEquals(
      filterWithFlatMap(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))(_ % 2 == 0),
      List(0, 2, 4, 6, 8)
    )
  }

  test("addPairWise accepts two lists and create a new one by adding corresponding elements") {
    assertEquals(
      sumPairWise(List(1, 2, 3), List(6, 7, 8)),
      List(7, 9, 11)
    )
    assertEquals(
      sumPairWise(List(1, 2), List(6, 7, 8)),
      List(7, 9)
    )
    assertEquals(
      sumPairWise(List(1, 2, 3), List(6, 7)),
      List(7, 9)
    )
  }

  test("zipWith accepts two lists and construct a new one by applying a function on corresponding elements") {
    assertEquals(
      zipWith(List(1, 2, 3), List(6, 7, 8))(_ + _),
      List(7, 9, 11)
    )
    assertEquals(
      zipWith(List("Rafael ", "Jordana E "), List("Fiume", "Fiume"))(_ + _),
      List("Rafael Fiume", "Jordana E Fiume")
    )
  }

  test("checks if a list starts with a subsequence") {
    assert(startWith(List(1, 2, 3), List(1)))
    assert(startWith(List(1, 2, 3), List(1, 2)))
    assert(!startWith(List(1, 2, 3), List(1, 3)))
    assert(!startWith(List(1, 2, 3), List(5)))
    assert(!startWith(List(1, 2, 3), List(1, 2, 3, 4)))
  }

  test("hasSubsequence checks if a List container another List as a subsequence") {
    assert(hasSubsequence(List(1, 2, 3, 4), List(1)))
    assert(hasSubsequence(List(1, 2, 3, 4), List(2, 3)))
    assert(hasSubsequence(Nil, Nil))
    assert(!hasSubsequence(List(1, 2, 3, 4), List(2, 2)))
  }

  test("startWith and hasSubsequence obeys the following properties") {
    /*
     xs startsWith Nil
     */
    val xs = List(4, 5, 6, 10)
    val ys = List(8, 9, 7)
    val zs = List(2)

    // xs startsWith Nil
    assert(startWith(xs, List.empty[Int]))

    // Nil startWith Nil
    assert(startWith(Nil, Nil))

    // (xs append ys) startsWith xs
    assert(startWith(append(xs, ys), xs))

    // (xs append ys append zs) hasSubsequence ys
    assert(hasSubsequence(append(zs, append(xs, ys)), zs))

    // xs hasSubsequence Nil
    assert(hasSubsequence(xs, List.empty[Int]))
  }

  test("sums all numbers in the list"):
    assertEquals(sum(List(1, -2, 3, 4, 0)), 6)
    assertEquals(sum(Nil), 0) // identity

  test("multiplies all numbers in the list"):
    assertEquals(multiply(List(1, 0, 3, 4)), 0)
    assertEquals(multiply(List(-1, 2, 3, 4)), -24)
    assertEquals(multiply(Nil), 1) // identity
