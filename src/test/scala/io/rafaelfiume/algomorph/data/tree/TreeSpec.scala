package io.rafaelfiume.algomorph.data.tree

import io.rafaelfiume.algomorph.data.tree.Tree.*
import munit.FunSuite

class TreeSpec extends FunSuite:

  val aTree: Tree[Int] = Branch(
    left = Branch(left = Leaf(1), right = Branch(left = Leaf(2), right = Leaf(3))),
    right = Branch(left = Leaf(7), right = Leaf(8))
  )

  val plusOneTree: Branch[Int] = Branch(
    left = Branch(left = Leaf(2), right = Branch(left = Leaf(3), right = Leaf(4))),
    right = Branch(left = Leaf(8), right = Leaf(9))
  )

  test("size counts the number of nodes (leaves and branches) in a tree") {
    assert(mySize(aTree) == 9)
  }

  test("maximum returns the biggest element in a tree") {
    assert(maximum(aTree) == 8)
  }

  test("depth returns the maximum path length from the root of a tree to any leaf") {
    assert(depth(aTree) == 4)
  }

  test("map transforms the elements of a tree applying a function to each one of its elements") {
    assert(map(aTree)(_ + 1) == plusOneTree)
  }

  test("fold reconstructs the tree") {
    assert(fold(aTree)(a => Leaf(a): Tree[Int])(Branch(_, _)) == aTree)
  }

  test("fsize counts the number of nodes (leaves and branches) in a tree") {
    assert(fsize(aTree) == 9)
  }

  test("fdepth returns the maximum path length from the root of a tree to any leaf") {
    assert(fdepth(aTree) == 4)
  }

  test("fmaximum returns the biggest element in a tree") {
    assert(fmaximum(Leaf(1)) == 1)
    assert(fmaximum(aTree) == 8)
  }

  test("fmap transforms the elements of a tree applying a function to each one of its elements") {
    assert(fmap(aTree)(_ + 1) == plusOneTree)
  }
