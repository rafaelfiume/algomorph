package red.book.ch03

import munit.Assertions.*
import munit.FunSuite
import red.book.ch03.List.*

class ListSpec extends FunSuite:

  test("tail removes the first element of a list") {
    assert(tail(List(1, 2, 3, 4)) == List(2, 3, 4))
    assert(tail(Nil) == Nil)
    assert(tail(List(1)) == Nil)
  }

  test("setHead replaces the first element of a list") {
    assert(setHead(List(1, 2, 3, 4), 9) == List(9, 2, 3, 4))
    assert(setHead(Nil, 9) == Nil)
  }

  test("drop drops the nth element of a list") {
    assert(drop(List(1, 2, 3, 4), 2) == List(3, 4))
    assert(drop(Nil, 1) == Nil)
    assert(drop(List(1, 2, 3, 4), 0) == List(1, 2, 3, 4))
  }

  test("dropWhile removes elements from the list while they match predicate") {
    assert(dropWhile(List(-1, -2, 0, 1, 2, 3, 4), (x: Int) => x < 0) == List(0, 1, 2, 3, 4))
    assert(dropWhile(List(1, 2, 3, 4), (x: Int) => x > 0) == Nil)
    assert(dropWhile(Nil, (x: Int) => x > 0) == Nil)
    assert(dropWhile(List(1, 2, 3, 4), (x: Int) => x < 0) == List(1, 2, 3, 4))
  }

  // stopped accepting Nil where it doesn't make sense

  test("init returns all but the last element of the list") {
    assert(init(List(1, 2, 3, 4)) == List(1, 2, 3))
    assert(init(List(1)) == Nil)
  }

  test("shortCircuitProduct calculates product by stop if it finds 0") {
    assert(shortCircuitProduct(List(1, 2, 3, 4)) == 24)
    assert(shortCircuitProduct(List(1, 0, 3, 4)) == 0)
  }

  test("passing Cons and Nil to foldRight returns the same list".ignore) { // implementation is pending
    assertEquals(
      foldRight_l1(List(1, 2, 3, 4), Nil: List[Int])(Cons(_, _)),
      List(1, 2, 3, 4)
    )
    assertEquals(
      foldRight_l2(List(1, 2, 3, 4), Nil: List[Int])(Cons(_, _)),
      List(1, 2, 3, 4)
    )
  }

  test("length calculates the length of a list") {
    assert(myLength(Nil) == 0)
    assert(myLength(List(1)) == 1)
    assert(myLength(List(1, 2, 3, 4)) == 4)
  }

  test("sumLeft, productLeft and lengthLeft do the usual stuff but with a foldLeft implementation") {
    assert(productLeft(List(1, 0, 3, 4)) == 0)
    assert(productLeft(List(1, 2, 3, 4)) == 24)

    assert(sumLeft(List(1, 2, 3, 4)) == 10)
    assert(sumLeft(List(1, 0, 3, 4)) == 8)

    assert(lengthLeft(List('R', 'a', 'f', 'a', 'e', 'l')) == 6)
    assert(lengthLeft(List('c')) == 1)
  }

  test("reverse returns the reverse of a list") {
    assertEquals(
      myReverse(List(1, 2, 3, 4)),
      List(4, 3, 2, 1)
    )
    assertEquals(
      myReverse(List(1)),
      List(1)
    )
  }

  test("myappend appends one list to another") {
    assertEquals(
      myappend(List(1, 2, 3), List(4, 5, 6)),
      List(1, 2, 3, 4, 5, 6)
    )
    assertEquals(
      myappend(Nil, List(1)),
      List(1)
    )
  }

  test("concat concatenates a list of list into a single list") {
    assertEquals(
      concat(List(List(1, 2), List(3, 4), List(5, 6, 7))),
      List(1, 2, 3, 4, 5, 6, 7)
    )
  }

  test("plus1 transforms a list of integers by adding 1 to each element") {
    assertEquals(
      plus1(List(1, 2, 3, 4, 5)),
      List(2, 3, 4, 5, 6)
    )
  }

  test("doubleToString turns each element in a list into a String") {
    assertEquals(
      doubleToString(List(1.0, 2.1, 3.8, 4.9)),
      List("1.0", "2.1", "3.8", "4.9")
    )
  }

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
      addPairWise(List(1, 2, 3), List(6, 7, 8)),
      List(7, 9, 11)
    )
    assertEquals(
      addPairWise(List(1, 2), List(6, 7, 8)),
      List(7, 9)
    )
    assertEquals(
      addPairWise(List(1, 2, 3), List(6, 7)),
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

  test("myStartWith checks if a list starts with a subsequence") {
    assert(myStartWith(List(1, 2, 3), List(1)))
    assert(myStartWith(List(1, 2, 3), List(1, 2)))
    assert(!myStartWith(List(1, 2, 3), List(1, 3)))
    assert(!myStartWith(List(1, 2, 3), List(5)))
    assert(!myStartWith(List(1, 2, 3), List(1, 2, 3, 4)))
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
    assert(myStartWith(xs, Nil: List[Int]))

    // Nil startWith Nil
    assert(myStartWith(Nil, Nil))

    // (xs append ys) startsWith xs
    assert(myStartWith(append(xs, ys), xs))

    // (xs append ys append zs) hasSubsequence ys
    assert(hasSubsequence(append(zs, append(xs, ys)), zs))

    // xs hasSubsequence Nil
    assert(hasSubsequence(xs, Nil: List[Int]))
  }
