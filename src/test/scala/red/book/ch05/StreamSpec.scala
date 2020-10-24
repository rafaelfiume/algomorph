package red.book.ch05

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import red.book.ch04._
import red.book.ch05.Stream._

import scala.{None => _, Option => _, Some => _}

class StreamSpec extends AnyFlatSpec with Matchers {

  "toList" should "force evaluation and convert stream to list" in {
    Stream(1,2,3,4,5).toList shouldBe List(1,2,3,4,5)
  }

  "take" should "return the first n elements of a stream" in {
    Stream(1,2,3,4,5).take(3).toList shouldBe List(1,2,3)
  }

  "drop" should "drop the first n elements of a stream" in {
    Stream(1,2,3,4,5).drop(3).toList shouldBe List(4,5)
    println(Stream(1,2,3,4,5).drop(4))
  }

  "takeWhile" should "take all starting elements of a stream that matches a predicate" in {
    Stream(1,2,3,4,5).takeWhile(_ < 3).toList shouldBe List(1,2)
  }

  "takeWhile_w_fold" should "take all starting elements of a stream that matches a predicate" in {
    Stream(1,2,3,4,5).takeWhile_w_fold(_ < 3).toList shouldBe List(1,2)
    println(Stream(1,2,3,4,5).takeWhile_w_fold(_ < 3))
  }

  "forAll" should "check if all the elements in a stream matches a given predicate" in {
    Stream(1,2,3,4,5).forAll(_ > 0) shouldBe true
    Stream(1,2,3,4,5).forAll(_ % 2 == 0) shouldBe false
  }

  "headOption" should "return first element or none if stream is empty" in {
    Stream(1,2).headOption shouldBe Some(1)
    Stream.empty.headOption shouldBe None
  }

  "map" should "apply function f to each element of a stream" in {
    Stream(1,2,3).map(_ % 2).toList shouldBe List(1,0,1)
    Stream.empty[Int].map (_ % 2).toList shouldBe Nil
  }

  "filter" should "keep in the stream only elements that match a given predicate" in {
    Stream(1,2,3,4,5).filter(_ % 2 == 0).toList shouldBe List(2,4)
    Stream.empty[Int].filter(_ < 0).toList shouldBe Nil
  }

  "append" should "append another stream in to the original one" in {
    Stream(4,6,7,8).append(Stream(9,0,6)).toList shouldBe List(4,6,7,8,9,0,6)
  }

  "flatMap" should "apply function f in to each element of a stream" in {
    Stream(6,7,8,9).flatMap(x => Stream(x,x)).toList shouldBe List(6,6,7,7,8,8,9,9)
  }

  "constant" should "return an infinite stream" in {
    constant("a").take(5).toList shouldBe List("a", "a", "a", "a", "a")
  }

  "from" should "generate an infinite n, n+1, etc, stream of elements" in {
    from(6).take(3).toList shouldBe List(6,7,8)
  }

  "fibs" should "generate an infinite stream of Fibonacci numbers" in {
    fibs().take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
  }

  "unfold" should "build a stream using an initial state and a function to generate the next state and value" in {
    unfold(6)(s => Some(s -> (s+1))).take(3).toList shouldBe List(6,7,8)
  }

  "unfoldViaMap" should "build a stream using an initial state and a function to generate the next state and value" in {
    unfoldViaMap(6)(s => Some(s -> (s+1))).take(3).toList shouldBe List(6,7,8)
  }

  "unfoldViaStreamFold" should "build a stream using an initial state and a function to generate the next state and value" in {
    unfoldViaStreamFold(6)(s => Some(s -> (s+1))).take(3).toList shouldBe List(6,7,8)
  }

  "unfoldViaFold" should "build a stream using an initial state and a function to generate the next state and value" in {
    unfoldViaFold(6)(s => Some(s -> (s+1))).take(3).toList shouldBe List(6,7,8)
  }

  "fibs_u" should "generate an infinite stream of Fibonacci numbers" in {
    fibs_u().take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
  }

  "from_u" should "generate an infinite n, n+1, etc, stream of elements" in {
    from_u(6).take(3).toList shouldBe List(6,7,8)
  }

  "constant_u" should "return an infinite stream" in {
    Stream.constant_u("a").take(5).toList shouldBe List("a", "a", "a", "a", "a")
  }

  "ones_u" should "continue to return ones forever" in {
    Stream.ones_u().take(5).toList shouldBe List(1,1,1,1,1)
  }

  "map_u" should "apply function f to each element of a stream" in {
    Stream(1,2,3).map_u(_ % 2).toList shouldBe List(1,0,1)
    Stream.empty[Int].map_u (_ % 2).toList shouldBe Nil
  }

  "take_u" should "return the first n elements of a stream" in {
    Stream(1,2,3,4,5).take_u(3).toList shouldBe List(1,2,3)
    Stream(1,2,3,4,5).take_u(1).toList shouldBe List(1)
    Empty.take_u(3).toList shouldBe Nil
  }

  "takeWhile_u" should "take all starting elements of a stream that matches a predicate" in {
    Stream(1,2,3,4,5).takeWhile_u(_ < 3).toList shouldBe List(1,2)
    Stream.empty[Int].takeWhile_u(_ < 3).toList shouldBe Nil
  }

  "zipWith" should "accept two streams and construct a new one by applying a function on corresponding elements" in {
    Stream(1,2,3,4,5).zipWith(Stream(6,7,8,9))(_ + _).toList shouldBe List(7,9,11,13)
    Stream(1,2,3,4,5).zipWith(Stream.empty[Int])(_ + _).toList shouldBe Nil
    Stream.empty[Int].zipWith(Stream(6,7,8,9))(_ + _).toList shouldBe Nil
  }

  "zip" should "create new stream containing the corresponding pair of elements from each stream as long as both streams have elements" in {
    Stream(1,2,3,4,5).zip(Stream(6,7,8,9)).toList shouldBe List((1,6), (2,7), (3,8), (4,9))
    Stream(1,2).zip(Stream.empty).toList shouldBe Nil
    Stream.empty.zip(Stream(6,7)).toList shouldBe Nil
  }

  "zipWithAll" should "do the same as zipWith but continue the traversal as long as either stream has elements" in {
    Stream(1,2,3,4,5).zipAllWith(Stream(6,7,8,9))((_,_)).toList shouldBe List((Some(1),Some(6)), (Some(2),Some(7)), (Some(3),Some(8)), (Some(4),Some(9)), (Some(5),None))
    Stream(1,2).zipAllWith(Stream.empty)((_,_)).toList shouldBe List((Some(1), None), (Some(2), None))
    Stream.empty.zipAllWith(Stream(6,7))((_,_)).toList shouldBe List((None, Some(6)), (None, Some(7)))
  }

  "zipAll" should "continue the traversal as long as either stream has elements" in {
    Stream(1,2,3,4,5).zipAll(Stream(6,7,8,9)).toList shouldBe List((Some(1),Some(6)), (Some(2),Some(7)), (Some(3),Some(8)), (Some(4),Some(9)), (Some(5),None))
    Stream(1,2).zipAll(Stream.empty).toList shouldBe List((Some(1), None), (Some(2), None))
    Stream.empty.zipAll(Stream(6,7)).toList shouldBe List((None, Some(6)), (None, Some(7)))
  }

  "startsWith" should "check if a list starts with a subsequence" in {
    Stream(1,2,3,4,5).startsWith(Stream(1,2,3)) shouldBe true
    Stream(1,2,3,4,5).startsWith(Stream(2,3)) shouldBe false
    Stream(1,2,3,4,5).startsWith(Empty) shouldBe true
    Stream(1,2,3).startsWith(Stream(1,2,3,4)) shouldBe false
    Empty.startsWith(Stream(1)) shouldBe false
  }

  "tails" should "return the tail of a stream" in {
    Stream(1,2,3).tails.map(_.toList).toList shouldBe List(List(1,2,3), List(2,3), List(3), Nil)
    Stream(3).tails.map(_.toList).toList shouldBe List(List(3), Nil)
  }

  "hasSubsequence" should "check if a Stream contains another Stream as subsequence" in {
    Stream(1,2,3).hasSubsequence(Stream(2,3)) shouldBe true
    Stream(1,2,3).hasSubsequence(Stream(3,4)) shouldBe false
  }

  "scanRight" should "return a stream with all the intermediate results" in {
    Stream(1,2,3,4,5,6).take(3).scanRight(0)(_ + _).toList shouldBe List(0+1+2+3, 2+3+0, 3+0, 0)
  }

  "scanLeft" should "return a stream with all the intermediate results" in {
    Stream(1,2,3,4,5,6).take(3).scanLeft(0)(_ + _).toList shouldBe List(0, 0+1, 0+1+2, 0+1+2+3)
  }

}
