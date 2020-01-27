package red.book.ch07

import java.util.concurrent.Executors

import red.book.ch07.Examples.{ intsops, paragraph }
import red.book.ch07.Par.{ sequence, unit }
import org.scalatest.{ FlatSpec, Matchers }

class ParSpec extends FlatSpec with Matchers {

  val es = Executors.newFixedThreadPool(10)

  "sequence" should "convert a list of Par's into a Par with a list" in {
    val pars = List(unit(1), unit(2), unit(3))

    val r = sequence(pars)

    assert(Par.equal(es)(r, unit(List(1, 2, 3))))
  }

  "parMap" should "combine N parallel computation" in {
    val elems = List(1,2,3,4)

    val r = Par.parMap(elems)(_ + 1)

    r(es).get shouldEqual List(2,3,4,5)
  }

  "parFilter" should "filter elements of a list in parallel" in {
    val elems = List(1,2,3,4)
    val onlyPairs: Int => Boolean = i => i % 2 == 0

    val r = Par.parFilter(elems)(onlyPairs)

    r(es).get shouldBe List(2,4)
  }

  "parFilter_2" should "filter elements of a list in parallel" in {
    val elems = List(1,2,3,4)
    val onlyPairs: Int => Boolean = i => i % 2 == 0

    val r = Par.parFilter_2(elems)(onlyPairs)

    r(es).get shouldBe List(2,4)
  }

  "intsops" should "perform a certain operation in a list of ints" in {
    intsops(List(1,2,3,4,5).toIndexedSeq)(_+_)(es).get shouldBe 15
    intsops(List(-1,-2,-10,-9,-8,-4).toIndexedSeq)((x1,x2) => if (x1 > x2) x1 else x2)(es).get shouldBe -1
  }

  "paragraphs" should "take a list of paragraphs and return the total number of words across all paragraphs" in {
    val list = List("Lorem Ipsum", "bla bla bla", "This is another paragraph")

    val r = paragraph(list)(es).get

    r shouldBe 9
  }

  "map3" should "lift a function with 3 arguments to work with Pars" in {
    val (a, b, c) = (unit(1), unit(2), unit(5))

    var r = Par.map3(a, b, c)(_ + _ + _)

    r(es).get shouldBe 8
  }

  "map4" should "lift a function with 4 arguments to work with Pars" in {
    val (a, b, c, d) = (unit(1), unit(2), unit(5), unit(2))

    var r = Par.map4(a, b, c, d)(_ * _ + _ + _)

    r(es).get shouldBe 9
  }

  "map5" should "lift a function with 5 arguments to work with Pars" in {
    val (a, b, c, d, e) = (unit(1), unit(2), unit(5), unit(10), unit(2))

    var r = Par.map5(a, b, c, d, e)(_ * _ + _ + _ / _)

    r(es).get shouldBe 12
  }

}
