package red.book.ch04

import red.book.ch04.Either._

import scala.util.control.NonFatal
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EitherSpec extends AnyFlatSpec with Matchers {

  "map" should "apply function f on the right side of an element" in {
    Right(2) map (_ * 3) shouldBe Right(6)
    left[Int]("error") map (_ * 3) shouldBe Left("error")
  }

  "orElse" should "return right value, or (in case of left), return the default  provided" in {
    Right(2) orElse Right(5) shouldBe Right(2)
    left[Int]("error") orElse Right(5) shouldBe Right(5)
  }

  "flatMap" should "apply function f (which return an Either) on the right side of the element" in {
    Right(2) flatMap (x => Right(x * 3)) shouldBe Right(6)
    Right(2) flatMap (_ => Left("error")) shouldBe Left("error")
    Left("error") flatMap (_ => Left("error")) shouldBe Left("error")
  }

  "map2" should "lift a function so it can be used with Either instances" in new EitherSpecContext {
    def agg(x: Int, y: Int) = x + y

    parseInt("2").map2(parseInt("4"))(agg) shouldBe Right(6)
    parseInt("bla").map2(parseInt("4"))(agg) shouldBe left("For input string: \"bla\"")
    parseInt("2").map2(parseInt("bla"))(agg) shouldBe left("For input string: \"bla\"")
  }

  "sequence" should "return an either list with the first error it finds (if there is one) from a list of either" in {
    sequence(List(Right(2), Right(3), Right(4), Right(5))) shouldBe Right(List(2,3,4,5))
    sequence(List(Right(2), Left("error"), Left("bla"))) shouldBe Left("error")
  }

  "sequence_via_traverse" should "return an either list with the first error it finds (if there is one) from a list of either" in {
    sequence_via_traverse(List(Right(2), Right(3), Right(4), Right(5))) shouldBe Right(List(2,3,4,5))
    sequence_via_traverse(List(Right(2), Left("error"), Left("bla"))) shouldBe Left("error")
  }

  "traverse" should "traverse over a list with a function that might fail" in new EitherSpecContext {
    traverse(List("1", "2", "3"))(parseInt) shouldBe Right(List(1, 2, 3))
    traverse(List("a", "f", "f"))(parseInt) shouldBe Left("For input string: \"a\"")
  }
}

class EitherSpecContext {

  def parseInt(s: String): Either[String, Int] = Try(s.toInt)

  def Try[A](a: => A) = {
    try Right(a)
    catch { case NonFatal(e) => Left(e.getMessage) }
  }

}
