package red.book.ch07

import java.util.concurrent.Executors

import red.book.ch07.Nonblocking.Par
import red.book.ch07.Nonblocking.Par.{ run => runn, _ }
import org.scalatest.{ FlatSpec, Matchers }

class NonBlockingSpec extends FlatSpec with Matchers {

  private val es = Executors.newFixedThreadPool(1)

  "choiceN" should "choose between two or more forking computation" in {
    val numbers = List(unit(10), unit(20), unit(50))

    val r1 = choiceN(unit(0))(numbers)
    val r2 = choiceN(unit(1))(numbers)
    val r3 = choiceN(unit(2))(numbers)

    runn(es)(r1) shouldBe 10
    runn(es)(r2) shouldBe 20
    runn(es)(r3) shouldBe 50
  }

  "choice" should "choose between two forking computation" in {
    val (ifTrue, ifFalse) = (unit(1), unit(2))

    val r1 = choiceViaChoiceN(unit(true))(ifTrue, ifFalse)
    val r2 = choiceViaChoiceN(unit(false))(ifTrue, ifFalse)

    runn(es)(r1) shouldBe 1
    runn(es)(r2) shouldBe 2
  }

  "choiceMap" should "choose between one of the forking computation" in {
    val numbers = Map("first" -> unit(10), "snd" -> unit(20), "3rd" -> unit(50))

    val r1 = choiceMap(unit("first"))(numbers)
    val r2 = choiceMap(unit("snd"))(numbers)
    val r3 = choiceMap(unit("3rd"))(numbers)

    runn(es)(r1) shouldBe 10
    runn(es)(r2) shouldBe 20
    runn(es)(r3) shouldBe 50
  }

  "flatMap" should "apply a function to a Par value" in {
    val numbers = Map("first" -> unit(10), "snd" -> unit(20), "3rd" -> unit(50))

    val r1 = flatMap(unit("first"))(numbers)

    runn(es)(r1) shouldBe 10
  }

  "choiceNChooser" should "choose between two or more forking computation" in {
    val numbers = List(unit(10), unit(20), unit(50))

    val r1 = choiceNChooser(unit(0))(numbers)
    val r2 = choiceNChooser(unit(1))(numbers)
    val r3 = choiceNChooser(unit(2))(numbers)

    runn(es)(r1) shouldBe 10
    runn(es)(r2) shouldBe 20
    runn(es)(r3) shouldBe 50
  }

  "choiceViaChooser" should "choose between two forking computation" in {
    val (ifTrue, ifFalse) = (unit(1), unit(2))

    val r1 = choiceViaChooser(unit(true))(ifTrue, ifFalse)
    val r2 = choiceViaChooser(unit(false))(ifTrue, ifFalse)

    runn(es)(r1) shouldBe 1
    runn(es)(r2) shouldBe 2
  }

  "join" should "flats two nested Par's" in {
    val nested: Par[Par[Char]] = unit(unit('a'))

    val r = join(nested)

    runn(es)(r) shouldBe 'a'
  }

  "joinViaFlatMap" should "flats two nested Par's" in {
    val nested: Par[Par[Char]] = unit(unit('a'))

    val r = joinViaFlatMap(nested)

    runn(es)(r) shouldBe 'a'
  }

  "flatMapViaJoin" should "apply a function to a Par value" in {
    val numbers = Map("first" -> unit(10), "snd" -> unit(20), "3rd" -> unit(50))

    val r1 = flatMapViaJoin(unit("first"))(numbers)

    runn(es)(r1) shouldBe 10
  }

}
