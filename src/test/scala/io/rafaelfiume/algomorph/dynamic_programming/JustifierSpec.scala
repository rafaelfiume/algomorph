package io.rafaelfiume.algomorph.dynamic_programming

import munit.ScalaCheckSuite
import org.scalacheck.{Gen, ShrinkLowPriority}
import org.scalacheck.Prop.*

class JustifierSpec extends ScalaCheckSuite with ShrinkLowPriority:

  test("justifies text"):
    val text = "The house is on fire; we found this hotwheels on the floor, and its name is Bombshaker 🔥"
    val result = Justifier.justify(text, 40)
    val expected = List(
      "The  house  is  on  fire;  we found this",
      "hotwheels  on the floor, and its name is",
      "Bombshaker 🔥                           "
    )
    assertEquals(result, expected)

  test("single word"):
    assertEquals(Justifier.justify("bees", 10), List("bees      "))

  test("last line left-justified"):
    val result = Justifier.justify("a b c d", 5)
    val expected = List(
      "a b c",
      "d    "
    )
    assertEquals(result, expected)

  test("empty text"):
    assertEquals(Justifier.justify("", 10), List("          "))

  test("text with multiple spaces"):
    val text = "         a         b  c       "
    val result = Justifier.justify(text, 5)
    val expected = List("a b c")
    assertEquals(result, expected)

  test("word longer than max width"): // real-world justification would break long words instead
    val longWord = "otorrinolaringologista"
    intercept[IllegalArgumentException] { Justifier.justify(longWord, 10) }

  test("all lines have exactly the specified width"):
    forAll(texts, widths) { (text, width) =>
      val result = Justifier.justify(text, width)
      assert(result.forall(_.length() == width))
    }

  test("justified text matches input"):
    forAll(texts, widths) { (text, width) =>
      val result = Justifier.justify(text, width).mkString(" ")
      val resultingWords = if text.isEmpty then List("") else result.split("\\s+").toList
      val originalWords = text.split("\\s+").toList
      assertEquals(resultingWords, originalWords)
    }

  def texts: Gen[String] = Gen
    .choose(0, 30)
    .flatMap { numLines => Gen.listOfN(numLines, lines) }
    .map(_.mkString("\n"))

  def lines: Gen[String] = Gen
    .choose(1, 15)
    .flatMap { numWords => Gen.listOfN(numWords, words) }
    .map(_.mkString(" "))

  def words: Gen[String] = Gen
    .choose(1, 7)
    .flatMap { numChars => Gen.listOfN(numChars, Gen.alphaNumChar) }
    .map(_.mkString(""))

  // widths are at least the size of the smallest generated word
  def widths: Gen[Int] = Gen.choose(7, 20)
