package red.book.ch03

import org.scalatest.{ FlatSpec, Matchers }
import red.book.ch03.List._

class ListSpec extends FlatSpec with Matchers {

  "x" should "be == 3" in {
    x shouldBe 3
  }

  "tail" should "remove the first element of a list" in {
    tail(List(1,2,3,4)) shouldBe List(2,3,4)
    tail(Nil) shouldBe Nil
    tail(List(1)) shouldBe Nil
  }

  "setHead" should "replace the first element of a list" in {
    setHead(List(1,2,3,4), 9) shouldBe List(9,2,3,4)
    setHead(Nil, 9) shouldBe Nil
  }

  "drop" should "drop the nth element of a list" in {
    drop(List(1,2,3,4), 2) shouldBe List(3,4)
    drop(Nil, 1) shouldBe Nil
    drop(List(1,2,3,4), 0) shouldBe List(1,2,3,4)
  }

  "dropWhile" should "remove elements from the list while they match predicate" in {
    dropWhile(List(-1, -2, 0, 1,2,3,4), (x: Int) => x < 0) shouldBe List(0,1,2,3,4)
    dropWhile(List(1,2,3,4), (x: Int) => x > 0) shouldBe Nil
    dropWhile(Nil, (x: Int) => x > 0) shouldBe Nil
    dropWhile(List(1,2,3,4), (x: Int) => x < 0) shouldBe List(1,2,3,4)
  }

  // stopped accepting Nil where it doesn't make sense

  "init" should "return all but the last element of the list" in {
    init(List(1,2,3,4)) shouldBe List(1,2,3)
    init(List(1)) shouldBe Nil
  }

  "shortCircuitProduct" should "calculate product by stop if it finds 0" in {
    shortCircuitProduct(List(1,2,3,4)) shouldBe 24
    shortCircuitProduct(List(1,0,3,4)) shouldBe 0
  }

  "passing Cons and Nil to foldRight" should "return the same list" ignore { // implementation is pending
    foldRight_l1(List(1,2,3,4), Nil:List[Int])(Cons(_, _)) shouldBe List(1,2,3,4)
    foldRight_l2(List(1,2,3,4), Nil:List[Int])(Cons(_, _)) shouldBe List(1,2,3,4)
  }

  "length" should "calculate the length of a list" in {
    myLength(Nil) shouldBe 0
    myLength(List(1)) shouldBe 1
    myLength(List(1,2,3,4)) shouldBe 4
  }

  "sumLeft, productLeft and lengthLeft" should "do the usual stuff but with a foldLeft implementation" in {
    productLeft(List(1,2,3,4)) shouldBe 24
    productLeft(List(1,0,3,4)) shouldBe 0

    sumLeft(List(1,2,3,4)) shouldBe 10
    sumLeft(List(1,0,3,4)) shouldBe 8

    lengthLeft(List('R', 'a', 'f', 'a', 'e', 'l')) shouldBe 6
    lengthLeft(List('c')) shouldBe 1
  }

  "reverse" should "return the reverse of a list" in {
    myReverse(List(1,2,3,4)) shouldBe List(4,3,2,1)
    myReverse(List(1)) shouldBe List(1)
  }

  "myappend" should "append one list to another" in {
    myappend(List(1,2,3), List(4,5,6)) shouldBe List(1,2,3,4,5,6)
    myappend(Nil, List(1)) shouldBe List(1)
  }

  "concat" should "concatenate a list of list into a single list" in {
    concat(List(List(1,2), List(3,4), List(5,6,7))) shouldBe List(1,2,3,4,5,6,7)
  }

  "plus1" should "transform a list of integers by adding 1 to each element" in {
    plus1(List(1,2,3,4,5)) shouldBe List(2,3,4,5,6)
  }

  "doubleToString" should "turn each element in a list into a String" in {
    doubleToString(List(1.0, 2.1, 3.8, 4.9)) shouldBe List("1.0", "2.1", "3.8", "4.9")
  }

  "map" should "generalize each element in a list while preserving its structure" in {
    map(List(1,2,3,4))(_.toString) shouldBe List("1","2","3","4")
    map(List(1,2,3,4))(_ - 1) shouldBe List(0,1,2,3)
  }

  "filter" should "remove all elements that don't satisfy a predicate" in {
    filter(List(0,1,2,3,4,5,6,7,8,9))(_ % 2 == 0) shouldBe List(0,2,4,6,8)
  }

  "flatMap" should "works like a map, but take a function that returns a list, and append that list in the final result" in {
    flatMap(List(1,2,3,4))(e => List(e, e)) shouldBe List(1,1,2,2,3,3,4,4)
  }

  "filterWithFlatMap" should "remove all elements that don't satisfy a predicate" in {
    filterWithFlatMap(List(0,1,2,3,4,5,6,7,8,9))(_ % 2 == 0) shouldBe List(0,2,4,6,8)
  }

  "addPairWise" should "accept two lists and create a new one by adding corresponding elements" in {
    addPairWise(List(1,2,3), List(6,7,8)) shouldBe List(7,9,11)
    addPairWise(List(1,2), List(6,7,8)) shouldBe List(7,9)
    addPairWise(List(1,2,3), List(6,7)) shouldBe List(7,9)
  }

  "zipWith" should "accept two lists and construct a new one by applying a function on corresponding elements" in {
    zipWith(List(1,2,3), List(6,7,8))(_ + _) shouldBe List(7,9,11)
    zipWith(List("Rafael ", "Jordana E "), List("Fiume", "Fiume"))(_ + _) shouldBe List("Rafael Fiume", "Jordana E Fiume")
  }

  "myStartWith" should "check if a list starts with a subsequence" in {
    myStartWith(List(1,2,3), List(1)) shouldBe true
    myStartWith(List(1,2,3), List(1, 2)) shouldBe true
    myStartWith(List(1,2,3), List(1,3)) shouldBe false
    myStartWith(List(1,2,3), List(5)) shouldBe false
    myStartWith(List(1,2,3), List(1,2,3,4)) shouldBe false
  }

  "hasSubsequence" should "check if a List container another List as a subsequence" in {
    hasSubsequence(List(1, 2, 3, 4), List(1)) shouldBe true
    hasSubsequence(List(1, 2, 3, 4), List(2, 3)) shouldBe true
    hasSubsequence(Nil, Nil) shouldBe true
    hasSubsequence(List(1, 2, 3, 4), List(2, 2)) shouldBe false
  }

  "startWith and hasSubsequence" should "obey the following properties" in {
    /*
     xs startsWith Nil
     */
    val xs = List(4,5,6,10)
    val ys = List(8,9,7)
    val zs = List(2)

    // xs startsWith Nil
    myStartWith(xs, Nil:List[Int]) shouldBe true

    // Nil startWith Nil
    myStartWith(Nil, Nil) shouldBe true

    // (xs append ys) startsWith xs
    myStartWith(append(xs, ys), xs)

    // (xs append ys append zs) hasSubsequence ys
    hasSubsequence(append(zs, append(xs, ys)), zs)

    // xs hasSubsequence Nil
    hasSubsequence(xs, Nil: List[Int]) shouldBe true

  }


}
