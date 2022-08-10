package fp.ds

import munit.Assertions.*
import munit.FunSuite

class BinTreeSpec extends FunSuite:

  private val ofABinTree = Tree(List(1, 2, 3, 4, 5, 6, 7, 8))

  test("build a perfect balanced tree") {
    assertEquals(
      Tree(List(1, 2, 3, 4, 5, 6, 7, 8)),
      Branch(
        1,
        Branch(2, Branch(3, Leaf, Leaf), Branch(4, Leaf, Leaf)),
        Branch(5, Branch(6, Leaf, Leaf), Branch(7, Leaf, Branch(8, Leaf, Leaf)))
      )
    )
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
    assertEquals(Tree.size(ofABinTree), 8)
  }

  test("depth returns the length of the longest path from a root to a leaf") {
    assertEquals(Tree.depth(ofABinTree), 4)
  }

  test("equal returns true if trees have the same values and same structure") {
    val a = Tree(List(1, 2, 3, 4, 5, 6, 7, 8))
    val b = Tree(List(1, 2, 3, 5, 4, 6, 7, 8))
    assert(Tree.equal(a, a))
    assert(!Tree.equal(a, b))
  }

  test("flip a tree") {
    val t = Tree(List(1, 2, 3, 4, 5, 6, 7))

    assertEquals(
      Tree.flip(t),
      Branch(
        1,
        Branch(5, Branch(7, Leaf, Leaf), Branch(6, Leaf, Leaf)),
        Branch(2, Branch(4, Leaf, Leaf), Branch(3, Leaf, Leaf))
      )
    )
  }

  test("flipEqual checks if the second tree is in the flipped form of the first tree") {
    val t = Tree(List(1, 2, 3, 4, 5, 6, 7))

    val flipped = Tree.flip(t)

    assert(Tree.flippedEqual(t, flipped))
    assert(!Tree.flippedEqual(t, t))
  }

  val aTraversableTree: Branch[Int] = Branch(
    1,
    Branch(2, Leaf, Leaf),
    Branch(5, Branch(9, Leaf, Leaf), Leaf)
  )

  test("preorder traversal visits the tree in Root->Left->Right order") {
    assertEquals(Tree.preorder(aTraversableTree), List(1, 2, 5, 9))
    assertEquals(Tree.preorderAcc(aTraversableTree), List(1, 2, 5, 9))
  }

  test("inorder traversal visits the tree in Left->Root->Right order") {
    assertEquals(Tree.inorder(aTraversableTree), List(2, 1, 9, 5))
    assertEquals(Tree.inorderAcc(aTraversableTree), List(2, 1, 9, 5))
  }

  test("postorder traversal visits the tree in Left->Right->Root order") {
    assertEquals(Tree.postorder(aTraversableTree), List(2, 9, 5, 1))
    assertEquals(Tree.postorderAcc(aTraversableTree), List(2, 9, 5, 1))
  }
