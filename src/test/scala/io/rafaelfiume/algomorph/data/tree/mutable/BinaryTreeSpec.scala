package io.rafaelfiume.algomorph.data.tree.mutable

import io.rafaelfiume.algomorph.data.tree.mutable.testkit.TreeContext
import munit.FunSuite

class BinaryTreeSpec extends FunSuite with TreeContext:

  test("build indexed ordered tree"):
    val tree = BinaryTree.make(Array(5, 4, 9, 2, 1, 7, -1, 8))
    assertEquals(
      Traversals.inOrder(tree.root).map(_.value).toList,
      expected = List(5, 4, 9, 2, 1, 7, -1, 8)
    )
