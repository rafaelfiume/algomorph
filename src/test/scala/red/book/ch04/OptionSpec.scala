package red.book.ch04

import red.book.ch04.Option._
import org.scalatest.{ FlatSpec, Matchers }

class OptionSpec extends FlatSpec with Matchers {

  "map" should "transform value if Some is defined" in {
    Some(5) map (_ * 2) shouldBe Some(10)
    (None:Option[Int]) map (_ * 2) shouldBe None
  }

  "flatMap" should "apply f, which may fail, to the Option if not None" in {
    Some(5) flatMap (v => Some(v * 2)) shouldBe Some(10)
    Some(5) flatMap(_ => None) shouldBe None
    (None:Option[Int]) flatMap (v => Some(v * 2)) shouldBe None
    (None:Option[Int]) flatMap (_ => None) shouldBe None
  }

  "getOrElse" should "return some value, if any, or else a default value" in {
    Some(5) getOrElse 3 shouldBe 5
    None    getOrElse 3 shouldBe 3
  }

  "orElse" should "return itself if a value is defined, or else the result of a default expression" in {
    Some(5) orElse Some(3) shouldBe Some(5)
    None    orElse Some(3) shouldBe Some(3)
  }

  "filter" should "return Some value if predicate is true, and None otherwise" in {
    Some(5) filter ((a: Int) => a > 1) shouldBe Some(5)
    Some(5) filter ((a: Int) => a % 2 == 0) shouldBe None
    None filter (_ => true) shouldBe None
  }

  "sequence" should "return an option list from a list of option" in new OptionSpecContext {
    sequence(List(Some(1), Some(2), Some(3), Some(4))) shouldBe Some(List(1,2,3,4))
    sequence(List(Some(1), Some(3), None   , Some(4))) shouldBe None
  }

  "traverse" should "map over a list using a function that might fail" in new OptionSpecContext {
    def parseInts(ints: List[String]): Option[List[Int]] = traverse(ints)(i => Try(i.toInt))

    parseInts(List("1", "2", "3", "4")) shouldBe Some(List(1,2,3,4))
    parseInts(List("ra")) shouldBe None
  }

  "sequence_via_traverse" should "return an option list from a list of option" in new OptionSpecContext {
    sequence_via_traverse(List(Some(1), Some(2), Some(3), Some(4))) shouldBe Some(List(1,2,3,4))
    sequence_via_traverse(List(Some(1), Some(3), None   , Some(4))) shouldBe None
  }

}

class OptionSpecContext {

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case _: Exception => None}

}
