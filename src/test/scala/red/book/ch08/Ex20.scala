package red.book.ch08

import red.book.ch03.{ Branch, Leaf, Tree }
import red.book.ch04.{ Option, _ }
import red.book.ch05.Stream
import red.book.ch08.Dsl._
import red.book.ch08.Gen.choose
import red.book.ch08.MoreGens._
import red.book.ch08.Prop.forAll

import scala.{ Either => _, None => _, Option => _, Some => _, Stream => _ }

object Ex20 extends App {

  val streams: Gen[Stream[Int]] = choose(-500, 500).streamOfN(choose(0, 10))
  val indexes: Gen[Int] = choose(0, 10)

  /**** Stream properties ****/

  println("take should take n elements from a stream")
  val takeProp1 = forAll(streams ** indexes) { case stream ** index =>

    (stream take index) ++ (stream drop index) equal stream
  }

  val takeProp2 = forAll(streams ** indexes) { case stream ** index =>
    val sSize = stream.toList.size

    where(sSize > index) {
      (stream take index) hasSize index
    } &&
      where(sSize <= index) {
        (stream take index) hasSize sSize
      }
  }
  Prop.run(takeProp1 && takeProp2)

  println("drop should drop n elements of a stream")
  val dropProp1 = forAll(streams ** indexes) { case stream ** index =>
    val sSize = stream.toList.size

    where(sSize > index) {
      (stream drop index) hasSize (sSize - index)
    } &&
      where(sSize <= index) {
        (stream drop index) hasSize 0
      }
  }
  Prop.run(dropProp1)

  println("filter should return only elements that match a predicate")
  val filterProp1 = forAll(streams ** intToBooleanGen) { case stream ** p =>
    val sSize = stream.toList.size

    (stream filter p) ++ (stream filter (!p(_))) hasSize sSize
  }

  val filterProp2 = forAll(streams ** intToBooleanGen) { case stream ** p =>

    val overfiltered = (stream filter p) ++ (stream filter (!p(_)))

    overfiltered containsOnlyElementsOf stream // here stream could have other elements that are not present in overfiltered
    stream containsOnlyElementsOf overfiltered // so we need this check to ensure all the original elements are present in the overfiltered stream
  }
  Prop.run(filterProp1 && filterProp2)

  println("unfold continues when state is some and terminates when is none")
  val lists = choose(-500, 500) listOfN choose(0, 20)

  val unfoldProp1 = forAll(lists) { list =>

    val stream: Stream[Int] = Stream.unfold(list) { // producing a stream from a list
      case Nil => None
      case h :: t => Option(h -> t)
    }

    stream.toList == list
  }
  Prop.run(unfoldProp1)

  /**** Tree fold property ****/

    // A very simple Tree generator: hardcoding int generator and the size of the tree. // TODO Improve this generator if feeling like that
  val trees: Gen[Tree[Int]] = {
    def toTree[A](l: List[A]): Tree[A] = l splitAt(l.size / 2) match {
      case (le, _) if le.size == 1 => Leaf(le.head)
      case (_, lr) if lr.size == 1 => Leaf(lr.head)
      case (le, lr) => Branch(toTree(le), toTree(lr))
    }
    val nonEmptyLists = choose(-500, 500) listOfN choose(1, 25)
    nonEmptyLists map toTree
  }

  println("Fold (right) should be able to reconstruct the tree")
  val foldProp = forAll(trees) { tree =>
    Tree.fold(tree)(l => Leaf(l):Tree[Int])((bl, br) => Branch(bl, br)) == tree
  }
  Prop.run(foldProp)

  implicit class StreamOps[A](s: Stream[A]) {
    def equal(s2: Stream[A]): Boolean = s.toList == s2.toList

    def hasSize(expected: Int): Boolean = s.toList.size == expected

    def containsOnlyElementsOf(s2: Stream[A]): Boolean = {
      val ss2 = s2.toList
      s.toList forall(ss2.contains(_))
    }
  }

}
