package data.tree.mutable

import data.tree.mutable.testkit.TreeContext
import munit.FunSuite
import _root_.testkit.syntax.OptionSyntax.*

class BinaryNodeSpec extends FunSuite with TreeContext:

  /* Navigation */

  test("first returns the first in-order traversal node"):
    val node = aTree()
    assert(BinaryNode.first(node).value == 1)
    assert(BinaryNode.first(node.right).value == 5)

  test("last returns the last in-order node"):
    val node = aTree()
    assert(BinaryNode.last(node).value == 7)
    assert(BinaryNode.last(node.left).value == 3)

  test("predecessor finds the previous in-order node"):
    val node = aTree()
    val pred3 = BinaryNode.predecessor(node.left.right)
    assert(pred3.someOrFail.value == 2)

    val pred6 = BinaryNode.predecessor(node.right)
    assert(pred6.someOrFail.value == 5)

    val pred1 = BinaryNode.predecessor(node.left.left) // 1 min
    assert(pred1.isEmpty)

  test("successor finds the next in-order node"):
    val node = aTree()
    val succ3 = BinaryNode.successor(node.left.right)
    assert(succ3.someOrFail.value == 4)

    val succ6 = BinaryNode.successor(node.right)
    assert(succ6.someOrFail.value == 7)

    val succ7 = BinaryNode.successor(node.right.right) // 7 (max)
    assert(succ7.isEmpty)

  /* Local Structural Changes */

  test("insert item before a node preserves traversal in-order"):
    val root = aTree()
    val node2 = root.left
    val node77 = BinaryNode(77, null, null, null, 1)

    BinaryNode.insertBefore(node2, node77): Unit

    assertEquals(Traversals.inOrder(root).map(_.value).toList, List(1, 77, 2, 3, 4, 5, 6, 7))

  test("insert item after a node preserves traversal in-order"):
    val root = aTree()
    val node2 = root.left
    val node77 = BinaryNode(77, null, null, null, 1)

    BinaryNode.insertAfter(node2, node77): Unit

    assertEquals(Traversals.inOrder(root).map(_.value).toList, List(1, 2, 77, 3, 4, 5, 6, 7))

  test("delete item"):
    val node = rightHeavyTree()

    BinaryNode.delete(node.left /* n2 */ ): Unit
    BinaryNode.delete(node.right.right /* n8 */ ): Unit

    assertEquals(Traversals.inOrder(node).map(_.value).toList, expected = List(4, 6, 7))
