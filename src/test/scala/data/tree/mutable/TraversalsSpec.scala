package data.tree.mutable

import data.tree.mutable.testkit.TreeContext
import munit.FunSuite

class TraversalsSpec extends FunSuite with TreeContext:

  test("traverse the tree in-order"):
    assertEquals(
      Traversals.inOrder(aTree()).map(_.value).toList,
      expected = List(1, 2, 3, 4, 5, 6, 7)
    )
    assertEquals(
      Traversals.inOrder(rightHeavyTree()).map(_.value).toList,
      expected = List(2, 4, 6, 7, 8)
    )

  test("traverse the tree in pre-order"):
    assertEquals(
      Traversals.preOrder(aTree()).map(_.value).toList,
      expected = List(4, 2, 1, 3, 6, 5, 7)
    )
    assertEquals(
      Traversals.preOrder(rightHeavyTree()).map(_.value).toList,
      expected = List(4, 2, 6, 8, 7)
    )

  test("traverse the tree in post-order"):
    assertEquals(
      Traversals.postOrder(aTree()).map(_.value).toList,
      expected = List(1, 3, 2, 5, 7, 6, 4)
    )
    assertEquals(
      Traversals.postOrder(rightHeavyTree()).map(_.value).toList,
      expected = List(2, 7, 8, 6, 4)
    )
