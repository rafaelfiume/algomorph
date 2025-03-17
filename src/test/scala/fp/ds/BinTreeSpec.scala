package fp.ds

import munit.Assertions.*
import munit.FunSuite

class BinTreeSpec extends FunSuite:

  private val aList = List(1, 2, 3, 4, 5, 6, 7, 8)
  private val aTree = Branch(
    1,
    Branch(2, left = Branch(3, Leaf, Leaf), right = Branch(4, Leaf, Leaf)),
    Branch(5, left = Branch(6, Leaf, Leaf), right = Branch(7, Leaf, Branch(8, Leaf, Leaf)))
  )

  test("build a perfect balanced tree") {
    assertEquals(Tree.apply(aList), aTree)
  }

  test("build a tree where size(tree) == 2(depth(tree))-1") {
    assertEquals(
      Tree.completeTree(1, 3),
      Branch(
        1,
        Branch(2, Branch(4, Leaf, Leaf), Branch(5, Leaf, Leaf)),
        Branch(3, Branch(6, Leaf, Leaf), Branch(7, Leaf, Leaf))
      )
    )
  }

  test("size returns the number of non-leaf nodes in a binary tree") {
    assertEquals(Tree.size(aTree), 8)
  }

  test("depth returns the length of the longest path from a root to a leaf") {
    assertEquals(Tree.depth(aTree), 4)
  }

  test("equal returns true if trees have the same values and same structure") {
    val a = Tree(List(1, 2, 4, 5, 6))
    val b = Tree(List(1, 2, 5, 4, 6))
    assert(Tree.equal(a, a))
    assert(!Tree.equal(a, b))
  }

  test("flip a tree") {
    assertEquals(
      Tree.flip(aTree),
      Branch(
        1,
        Branch(5, Branch(7, Branch(8, Leaf, Leaf), Leaf), Branch(6, Leaf, Leaf)),
        Branch(2, Branch(4, Leaf, Leaf), Branch(3, Leaf, Leaf))
      )
    )
  }

  test("check if one tree is the flipped form of another") {
    val flipped = Tree.flip(aTree)

    assert(Tree.flippedEqual(aTree, flipped))
    assert(!Tree.flippedEqual(aTree, aTree))
  }

  test("preorder traversal visits the tree in Root->Left->Right order") {
    assertEquals(Tree.preorder(aTree), List(1, 2, 3, 4, 5, 6, 7, 8))
    assertEquals(Tree.preorderAcc(aTree), List(1, 2, 3, 4, 5, 6, 7, 8))
  }

  test("inorder traversal visits the tree in Left->Root->Right order") {
    assertEquals(Tree.inorder(aTree), List(3, 2, 4, 1, 6, 5, 7, 8))
    assertEquals(Tree.inorderAcc(aTree), List(3, 2, 4, 1, 6, 5, 7, 8))
  }

  test("postorder traversal visits the tree in Left->Right->Root order") {
    assertEquals(Tree.postorder(aTree), List(3, 4, 2, 6, 8, 7, 5, 1))
    assertEquals(Tree.postorderAcc(aTree), List(3, 4, 2, 6, 8, 7, 5, 1))
  }
