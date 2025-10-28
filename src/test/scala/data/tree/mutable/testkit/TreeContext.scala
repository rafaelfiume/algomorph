package data.tree.mutable.testkit

import data.tree.mutable.BinaryNode

trait TreeContext:

  //        4
  //       / \
  //      2   6
  //     / \ / \
  //    1  3 5  7
  def aTree(): BinaryNode[Int] =
    val n1 = BinaryNode(1, null, null, null, 0)
    val n3 = BinaryNode(3, null, null, null, 0)
    val n5 = BinaryNode(5, null, null, null, 0)
    val n7 = BinaryNode(7, null, null, null, 0)
    val n2 = BinaryNode(2, n1, n3, null, 1)
    val n6 = BinaryNode(6, n5, n7, null, 1)
    val root = BinaryNode(4, n2, n6, null, 2)
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
  def rightHeavyTree(): BinaryNode[Int] =
    val n2 = BinaryNode(2, null, null, null, 0)
    val n7 = BinaryNode(7, null, null, null, 0)
    val n8 = BinaryNode(8, n7, null, null, 1)
    val n6 = BinaryNode(6, null, n8, null, 2)
    val root = BinaryNode(4, n2, n6, null, 3)
    n8.parent = n6
    n7.parent = n8
    n2.parent = root
    n6.parent = root
    root
