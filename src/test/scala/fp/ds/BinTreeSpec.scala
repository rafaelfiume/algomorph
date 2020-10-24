package fp.ds

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BinTreeSpec extends AnyFlatSpec with Matchers {

  private val ofABinTree = Tree(List(1, 2, 3, 4, 5, 6, 7, 8))

  "Tree" should "be able to build a perfect balanced tree" in {
    Tree(List(1,2,3,4,5,6,7,8)) shouldBe Branch(
      1,
      Branch(2,Branch(3,Leaf,Leaf),Branch(4,Leaf,Leaf)),
      Branch(5,Branch(6,Leaf,Leaf),Branch(7,Leaf,Branch(8,Leaf,Leaf)))
    )
  }

  "completeTree" should "build a tree where size(tree) == 2(depth(tree))-1" in {
    Tree.completeTree(1, 3) shouldBe Branch(
      1,
      Branch(2,Branch(4,Leaf,Leaf),Branch(5,Leaf,Leaf)),
      Branch(3,Branch(6,Leaf,Leaf),Branch(7,Leaf,Leaf))
    )
  }

  "size" should "return the number of non-leaf nodes in a binary tree" in {
    Tree.size(ofABinTree) shouldBe 8
  }

  "depth" should "return the length of the longest path from a root to a leaf" in {
    Tree.depth(ofABinTree) shouldBe 4
  }

  "equal" should "returns true if trees have the same values and same structure" in {
    val a = Tree(List(1,2,3,4,5,6,7,8))
    val b = Tree(List(1,2,3,5,4,6,7,8))
    Tree.equal(a, a) shouldBe true
    Tree.equal(a, b) shouldBe false
  }

  "flip" should "flip a tree" in {
    val t = Tree(List(1,2,3,4,5,6,7))

    Tree.flip(t) shouldBe Branch(
      1,
      Branch(5,Branch(7,Leaf,Leaf),Branch(6,Leaf,Leaf)),
      Branch(2,Branch(4,Leaf,Leaf),Branch(3,Leaf,Leaf))
    )
  }

  "flipEqual" should "check if the second tree is in the flipped form of the first tree" in {
    val t = Tree(List(1,2,3,4,5,6,7))
    val flipped = Tree.flip(t)

    Tree.flippedEqual(t, t) shouldBe false
    Tree.flippedEqual(t, flipped) shouldBe true
  }

  val aTraversableTree = Branch(
    1,
    Branch(2,Leaf,Leaf),
    Branch(
      5,
      Branch(9,Leaf,Leaf), Leaf)
  )

  "preorder traversal" should "visit the tree in Root->Left->Right order" in {
    Tree.preorder(aTraversableTree) shouldBe List(1,2,5,9)
    Tree.preorderAcc(aTraversableTree) shouldBe List(1,2,5,9)
  }

  "inorder traversal" should "visit the tree in Left->Root->Right order" in {
    Tree.inorder(aTraversableTree) shouldBe List(2,1,9,5)
    Tree.inorderAcc(aTraversableTree) shouldBe List(2,1,9,5)
  }

  "postorder traversal" should "visit the tree in Left->Right->Root order" in {
    Tree.postorder(aTraversableTree) shouldBe List(2,9,5,1)
    Tree.postorderAcc(aTraversableTree) shouldBe List(2,9,5,1)
  }
}
