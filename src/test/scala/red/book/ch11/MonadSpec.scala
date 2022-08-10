package red.book.ch11

import munit.Assertions.*
import munit.FunSuite
import red.book.ch11.Monads.*

class MonadSpec extends FunSuite:

  test("sequencing a list of Option's returns None where there's at leas one None element in the list") {
    val options = List(Some(1), Some(2), None, Some(4), Some(5), Some(6))

    val res = optionMonad.sequence(options)

    assertEquals(res, None)
  }

  test("sequencing a list of Option's returns Some list when list contains only Some elements") {
    val options = List(Some(1), Some(2), Some(3), Some(4), Some(5), Some(6))

    val res = optionMonad.sequence(options)

    assertEquals(res, Some(List(1, 2, 3, 4, 5, 6)))
  }

  test("traverse ---- describe what traverse does here -----") {
    val ints = List(1, 2, 3, 4, 5, 6)

    val res = optionMonad.traverse(ints)(i => Some(i.toString))

    assertEquals(res, Some(List("1", "2", "3", "4", "5", "6")))
  }

  test("traverse ---- describe what traverse does here ----- (Part ll)") {
    val ints = List(1, 2, 3, 4, 5, 6)

    val res = optionMonad.traverse(ints)(i => if i % 2 == 0 then None else Some(i))

    assertEquals(res, None)
  }

  test("replicateM respects a Monad behaviour") {
    val none = None

    val res = optionMonad.replicateM(4, none)

    assertEquals(res, None)
  }

  test("replicate an element inside a Monad n times") {
    val option = Some(1)

    val res = optionMonad.replicateM(5, option)

    assertEquals(res, Some(List(1, 1, 1, 1, 1)))
  }

  test("replicates works with lists too") {
    val list = List(8, 9)

    val res = listMonad.replicateM(2, list)

    assertEquals(res, List(List(8, 8), List(8, 9), List(9, 8), List(9, 9)))
  }

  test("filter elements in a list") {
    val list = List(1, 2, 3, 4, 5)
    val p: Int => Option[Boolean] = i => if i % 2 == 0 then Some(true) else Some(false)

    val res = optionMonad.filterM(list)(p)

    assertEquals(res, Some(List(2, 4)))
  }

  test("filter elements in a list - part ll") {
    val list = List(1, 2, 3, 4, 5)
    val p: Int => List[Boolean] = i => if i % 2 == 0 then List(true) else List(false)

    val res = listMonad.filterM(list)(p)

    assertEquals(res, List(List(2, 4)))
  }

  test("composes monadic functions") {
    val fa: Int => Option[Int] = i => Some(i * 3)
    val fb: Int => Option[String] = i => Some(i.toString)

    val res = optionMonad.compose(fa, fb)(5)

    assertEquals(res, Some("15"))
  }

  test(
    "compose respects its context (meaning that it could interrupt the computation in case the context is Option, for example)"
  ) {
    val fa: Int => Option[Int] = _ => None
    val fb: Int => Option[String] = _ => Some("Don't care, really!")

    val res = optionMonad.compose(fa, fb)(15)

    assertEquals(res, None)
  }

  // 11.12
  test("join flattens options") {
    assertEquals(optionMonad.join(Some(Some(5))), Some(5))
    assertEquals(optionMonad.join(Some(None)), None)
  }

  // 11.12
  test("join flats a list") {
    val listOfLists = List(List(1, 2, 3), List(4), List(5, 6))

    val res = listMonad.join(listOfLists)

    assertEquals(res, List(1, 2, 3, 4, 5, 6))
  }

  // 11.17
  test("a monad instance of Id defines the id function") {
    val value = 2

    val res = idMonad.unit(2)

    assertEquals(res, Id(value))
  }

  // 11.17
  test("a monad instance of Id defines the flatMap function") {
    val value = 2

    val res = idMonad.flatMap(Id(value))(x => Id(x * 3))

    assertEquals(res, Id(value * 3))
  }

// 11.18
