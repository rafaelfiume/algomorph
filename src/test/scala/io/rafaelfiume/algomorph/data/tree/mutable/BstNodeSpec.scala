package io.rafaelfiume.algomorph.data.tree.mutable

import io.rafaelfiume.algomorph.data.tree.mutable.testkit.TreeContext
import munit.FunSuite

class BstNodeSpec extends FunSuite with TreeContext:

//   test("find subtree"):
//     val bst = toBst(aTree())
//     println(bst.inOrder.map(_.toString()).toList)
  List(
    (1, true),
    (2, true),
    (3, true),
    (4, true),
    (5, true),
    (6, true),
    (7, true),
    (0, false),
    (99, false)
  ).foreach { case (key, exists) =>
    test(s"find returns subtree with key '$key' if it exists".ignore):
      val tree = toBst(aTree())

      val result = BstNode.find(key, tree)

      assertEquals(result.isDefined, exists)
      if exists then
        val node = result.get
        assertEquals(node.key, key)
    }


