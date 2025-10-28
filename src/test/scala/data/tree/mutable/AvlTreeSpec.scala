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
    val n1 = AvlTree(1, null, null, null, 1)
    val n3 = AvlTree(3, null, null, null, 1)
    val n5 = AvlTree(5, null, null, null, 1)
    val n7 = AvlTree(7, null, null, null, 1)
    val n2 = AvlTree(2, n1, n3, null, 2)
    val n6 = AvlTree(6, n5, n7, null, 2)
    val root = AvlTree(4, n2, n6, null, 3)
    n1.parent = n2; n3.parent = n2
    n5.parent = n6; n7.parent = n6
    n2.parent = root; n6.parent = root
    root

  test("traverse the tree in-order" ):
    val root = aTree()
    val result = traverseInOrder(root).map(_.value).toList
    assertEquals(result, List(1, 2, 3, 4, 5, 6, 7))

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

//   test("first and last on single node tree") {
//     val single = AvlTree(42, null, null, null, 1)
//     assert(first(single).value == 42)
//     assert(last(single).value == 42)
//     assert(successor(single).isEmpty)
//   }
