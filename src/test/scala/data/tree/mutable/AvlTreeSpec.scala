package data.tree.mutable

import AvlTree.*
import munit.FunSuite
import testkit.syntax.OptionSyntax.*

class AvlTreeSpec extends FunSuite:

  //        4
  //       / \
  //      2   6
  //     / \ / \
  //    1  3 5  7
  private def aTree(): AvlTree[Int] =
    val n1 = AvlTree(1, null, null, null, 0)
    val n3 = AvlTree(3, null, null, null, 0)
    val n5 = AvlTree(5, null, null, null, 0)
    val n7 = AvlTree(7, null, null, null, 0)
    val n2 = AvlTree(2, n1, n3, null, 1)
    val n6 = AvlTree(6, n5, n7, null, 1)
    val root = AvlTree(4, n2, n6, null, 2)
    n1.parent = n2; n3.parent = n2
    n5.parent = n6; n7.parent = n6
    n2.parent = root; n6.parent = root
    root

  //        4
  //       / \
  //      2   6
  //           \
  //            8
  //           /
  //          7
  private def rightHeavyTree(): AvlTree[Int] =
    val n2 = AvlTree(2, null, null, null, 0)
    val n7 = AvlTree(7, null, null, null, 0)
    val n8 = AvlTree(8, n7, null, null, 1)
    val n6 = AvlTree(6, null, n8, null, 2)
    val root = AvlTree(4, n2, n6, null, 3)
    n8.parent = n6
    n7.parent = n8
    n2.parent = root
    n6.parent = root
    root

  /* Traversals */

  test("traverse the tree in-order"):
    assertEquals(
      traverseInOrder(aTree()).map(_.value).toList,
      expected = List(1, 2, 3, 4, 5, 6, 7)
    )
    assertEquals(
      traverseInOrder(rightHeavyTree()).map(_.value).toList,
      expected = List(2, 4, 6, 7, 8)
    )

  test("traverse the tree in pre-order"):
    assertEquals(
      traversePreOrder(aTree()).map(_.value).toList,
      expected = List(4, 2, 1, 3, 6, 5, 7)
    )
    assertEquals(
      traversePreOrder(rightHeavyTree()).map(_.value).toList,
      expected = List(4, 2, 6, 8, 7)
    )

  test("traverse the tree in post-order"):
    assertEquals(
      traversePostOrder(aTree()).map(_.value).toList,
      expected = List(1, 3, 2, 5, 7, 6, 4)
    )
    assertEquals(
      traversePostOrder(rightHeavyTree()).map(_.value).toList,
      expected = List(2, 7, 8, 6, 4)
    )

  /* Navigation */

  test("first returns the first in-order traversal node"):
    val root = aTree()
    assert(first(root).value == 1)
    assert(first(root.right).value == 5)

  test("last returns the last in-order node"):
    val root = aTree()
    assert(last(root).value == 7)
    assert(last(root.left).value == 3)

  test("predecessor finds the previous in-order node"):
    val root = aTree()
    val pred3 = predecessor(root.left.right)
    assert(pred3.someOrFail.value == 2)

    val pred6 = predecessor(root.right)
    assert(pred6.someOrFail.value == 5)

    val pred1 = predecessor(root.left.left) // 1 min
    assert(pred1.isEmpty)

  test("successor finds the next in-order node"):
    val root = aTree()
    val succ3 = successor(root.left.right)
    assert(succ3.someOrFail.value == 4)

    val succ6 = successor(root.right)
    assert(succ6.someOrFail.value == 7)

    val succ7 = successor(root.right.right) // 7 (max)
    assert(succ7.isEmpty)

  /* Balancing-related Ops */

  test("insert item before a node preserves traversal in-order"):
    val root = aTree()
    val node2 = root.left
    val node77 = AvlTree(77, null, null, null, 1)
    val result = insertBefore(node2, node77)
    assertEquals(traverseInOrder(result).map(_.value).toList, List(1, 77, 2, 3))

  test("insert item after a node preserves traversal in-order"):
    val root = aTree()
    val node2 = root.left
    val node77 = AvlTree(77, null, null, null, 1)
    val result = insertAfter(node2, node77)
    assertEquals(traverseInOrder(result).map(_.value).toList, List(1, 2, 77, 3))
