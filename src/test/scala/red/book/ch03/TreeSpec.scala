package red.book.ch03

import org.scalatest.{ FlatSpec, Matchers }
import red.book.ch03.Tree._

class TreeSpec extends FlatSpec with Matchers {

  "size" should "count the number of nodes (leaves and branches) in a tree" in {
     mySize(aTree) shouldBe 9
  }

  "maximum" should "return the biggest element in a tree" in {
    maximum(aTree) shouldBe 8
  }

  "depth" should "return the maximum path length from the root of a tree to any leaf" in {
    depth(aTree) shouldBe 4
  }

  "map" should "transform the elements of a tree applying a function to each one of its elements" in {
    map(aTree)(_ + 1) shouldBe plusOneTree
  }

  "fold" should "be able to reconstruct tree" in {
    fold(aTree)(a => Leaf(a): Tree[Int])(Branch(_, _)) shouldBe aTree
  }

  "fsize" should "count the number of nodes (leaves and branches) in a tree" in {
    fsize(aTree) shouldBe 9
  }

  "fdepth" should "return the maximum path length from the root of a tree to any leaf" in {
    fdepth(aTree) shouldBe 4
  }

  "fmaximum" should "return the biggest element in a tree" in {
    fmaximum(Leaf(1)) shouldBe 1
    fmaximum(aTree) shouldBe 8
  }

  "fmap" should "transform the elements of a tree applying a function to each one of its elements" in {
    fmap(aTree)(_ + 1) shouldBe plusOneTree
  }

  val aTree: Tree[Int] = Branch(
    left = Branch(
      left = Leaf(1),
      right = Branch(left = Leaf(2), right = Leaf(3))),
    right = Branch(
      left = Leaf(7),
      right = Leaf(8))
  )

  val plusOneTree = Branch(
    left = Branch(
      left = Leaf(2),
      right = Branch(left = Leaf(3), right = Leaf(4))),
    right = Branch(
      left = Leaf(8),
      right = Leaf(9))
  )
}
