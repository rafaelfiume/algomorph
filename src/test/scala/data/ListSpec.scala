package data

import munit.Assertions.*
import munit.FunSuite
import data.List.*

class ListSpec extends FunSuite:

  test("returns the size of a list"):
    assertEquals(size(List(1, 2, 3, 4)), expected = 4)
    assertEquals(size(List("a", "b")), expected = 2)
    assertEquals(size(Nil), expected = 0)

  test("init returns all but the last element of the list"):
    assertEquals(init(List(1, 2, 3, 4)), expected = List(1, 2, 3))
    assertEquals(init(List(1)), expected = Nil)

  test("tail removes the first element of the list"):
    assertEquals(tail(List(1, 2, 3, 4)), expected = List(2, 3, 4))
    assertEquals(tail(Nil), expected = Nil)
    assertEquals(tail(List(1)), expected = Nil)

  test("setHead replaces the first element of a list"):
    assertEquals(setHead(List(1, 2, 3, 4), 9), expected = List(9, 2, 3, 4))
    assertEquals(setHead(List(1), 9), expected = List(9))
    assertEquals(setHead(Nil, 9), expected = Nil)

  test("appends one list to another") {
    assertEquals(append(List(1, 2, 3), List(4, 5, 6)), expected = List(1, 2, 3, 4, 5, 6))
    assertEquals(append(Nil, List(1)), expected = List(1))
  }

  test("flattens the list"):
    assertEquals(flatten(List(List(1, 2), List(3, 4), List(5, 6, 7))), expected = List(1, 2, 3, 4, 5, 6, 7))
    assertEquals(flatten(List(Nil, List(3, 4))), expected = List(3, 4))
    assertEquals(flatten(Nil), expected = Nil)

  test("reverses the list"):
    assertEquals(reverse(List(1, 2, 3, 4)), expected = List(4, 3, 2, 1))
    assertEquals(reverse(List(1)), expected = List(1))
    assertEquals(reverse(Nil), expected = Nil)

  test("map transforms the list preserving its structure"):
    assertEquals(map(List(1, 2, 3, 4))(_.toString), expected = List("1", "2", "3", "4"))
    assertEquals(map(List(1, 2, 3, 4))(_ - 1), expected = List(0, 1, 2, 3))

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
    // ((0 - 1) - 2) - 3 = -6
    val input = List(1, 2, 3)
    val result = foldl(input, 0)(_ - _)
    assertEquals(result, -6)

  test("foldLeft is stack-safe"):
    val largeInput = fill(100_000)(1)
    val result = foldl(largeInput, 0)(_ + _)
    assertEquals(result, 100_000)

  test("foldRight reconstructs the original list"):
    val input = List(1, 2, 3, 4)
    val result = foldr(input, List.empty[Int])(Cons(_, _))
    assertEquals(result, input)

  test("foldRight is stack-safe"):
    val largeInput = fill(100_000)(1)
    val result = foldr(largeInput, 0)(_ + _)
    assertEquals(result, 100_000)

  test("foldRight is right-associative (ops applied in reverse order)"):
    // 1 - (2 - (3 - 0)) = 2
    val input = List(1, 2, 3)
    val result = foldr(input, 0)(_ - _)
    assertEquals(result, 2)

  test("filter builds a new list containing only elements that satisfy the predicate"):
    assertEquals(filter(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))(_ % 2 == 0), List(0, 2, 4, 6, 8))

  test("finds the largest element"):
    assertEquals(max(List(5)), expected = 5)
    assertEquals(max(List(1, 5, 3, 4, -1)), expected = 5)

  test("flatMap works like a map, but take a function that returns a list, and append that list in the final result") {
    assertEquals(
      flatMap(List(1, 2, 3, 4))(e => List(e, e)),
      List(1, 1, 2, 2, 3, 3, 4, 4)
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
      zipWith(List("Rafael ", "Joana E "), List("Fiume", "Fiume"))(_ + _),
      List("Rafael Fiume", "Joana E Fiume")
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
    assertEquals(sum(List(1, -2, 3, 4, 0)), expected = 6)
    assertEquals(sum(Nil), expected = 0) // identity

  test("multiplies all numbers in the list"):
    assertEquals(multiply(List(1, 0, 3, 4)), expected = 0)
    assertEquals(multiply(List(-1, 2, 3, 4)), expected = -24)
    assertEquals(multiply(Nil), expected = 1) // identity

  test("remove duplicates"):
    assertEquals(removeDups(List('a', 'b', 'c', 'c', 'a')), expected = List('a', 'b', 'c'))
    assertEquals(size(removeDups(List.empty)), expected = 0)

  test("returns nth to last element"):
    assertEquals(nthToLast(List(1, 2, 3), k = 1), expected = 3)
    assertEquals(nthToLast(List(1, 2, 3), k = 2), expected = 2)
    assertEquals(nthToLast(List(1, 2, 3), k = 3), expected = 1)

  test("partitions the list"):
    assertEquals(partition(List(10, 8, 1, 2, 5, 5, 3), 5), expected = List(1, 2, 3, 10, 8, 5, 5))
    assertEquals(partition(Nil, 5), Nil)

  test("sums lists"):
    assertEquals(sum(List(9, 8, 7), List(4, 6, 9)), expected = List(3, 5, 7, 1)) // basic: 789 + 964 = 1753
    assertEquals(sum(List(9, 9), List(0, 0, 1)), expected = List(9, 9, 1)) // uneven length: 99 + 100 = 199
    assertEquals(sum(List(1), List.empty[Int]), expected = List(1)) // one empty list
    assertEquals(sum(List.empty[Int], List.empty[Int]), expected = Nil) // both empty
    intercept[IllegalArgumentException] { sum(List(-1), List(2)) } // rejects negative digits

  test("checks if palindrome"):
    assert(isPalindrome(List('a', 'b', 'c', 'b', 'a'))) // palindrome
    assert(isPalindrome(List('a', 'b', 'b', 'a'))) // palindrome
    assert(isPalindrome(List(1, 1))) // minimum palindrome
    assert(isPalindrome(List(1))) // single element is palindrome
    assert(isPalindrome(Nil)) // empty is palindrome
    assert(!isPalindrome(List('j', 'o', 'a', 'n', 'a'))) // not a palindrome
    assert(!isPalindrome(List(1, 2))) // minimum non-palindrome

  test("finds intersection by reference"):
    val tail = List(7, 2, 1)
    val a = append(List(3, 1, 5, 9), tail)
    val b = append(List(4, 6), tail)
    val c = List(3, 1, 5, 9, 7, 2, 1)
    val d = tail
    assertEquals(findIntersection(a, b), expected = Some(tail)) // intersection
    assertEquals(findIntersection(c, b), expected = None) // same values, but different nodes
    assertEquals(findIntersection(left = append(List(5), d), right = d), expected = Some(d)) // intersection of sublists
    assertEquals(findIntersection(Nil, Nil), expected = None)
    assertEquals(findIntersection(List('c'), List('c')), expected = None) // different nodes
