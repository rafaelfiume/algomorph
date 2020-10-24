package red.book.ch10

import red.book.ch03.Tree.fold
import red.book.ch03.{ Branch, Leaf, Tree }
import red.book.ch08.{ Gen, Prop }
import red.book.ch08.Gen.{ choose, stringN }
import red.book.ch08.Prop.forAll
import red.book.ch10.Foldable.{ foldableList, foldableTree }
import red.book.ch10.Monoids.intAddition
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

object FoldableSpec extends App {

  ///////////// List foldable properties /////////////

  val foldableListProps = forAll(stringLists) { list =>
    foldableList.foldRight(list)(List.empty[String])(_ :: _) == list

  } && forAll(stringLists) { list =>
    foldableList.foldLeft(list)(List.empty[String])((l, e) => e :: l) == list.reverse

  } && forAll(stringLists) { list =>
    foldableList.foldMap(list)(_.length)(intAddition) == list.map(_.length).fold(intAddition.zero)(intAddition.op)
  }

  Prop.run(foldableListProps)

  def ssize: Gen[Int] = choose(0, 40)

  def stringLists: Gen[List[String]] = for {
     ls <- ssize
     ss <- ssize
     list <- Gen.listOfN(ls, stringN(ss))
  } yield list

}

class TreeFoldableSpec extends AnyFlatSpec with Matchers {

//  "Tree foldable" should "be able to reconstruct tree" in {
//    def zz: Tree[Int] => Tree[Int] = t => t // the id function
//
//    def ff(v: Int, t: Tree[Int]): Tree[Int] => Tree[Int] = t match {
//      case Leaf(v) => t => Leaf(v)
//      case Branch(l, f) => t => Branch(l, f)
//    }
//    val r: Tree[Int] = foldableTree(aTree)(zz)(zz)
//    r shouldBe aTree
//  }

  val aTree: Tree[Int] = Branch(
    left = Branch(
      left = Leaf(1),
      right = Branch(left = Leaf(2), right = Leaf(3))),
    right = Branch(
      left = Leaf(7),
      right = Leaf(8))
  )

}