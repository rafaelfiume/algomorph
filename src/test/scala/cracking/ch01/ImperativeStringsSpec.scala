package cracking.ch01

import java.util

import cracking.ch01.ImperativeStrings._
import org.scalatest.{ FlatSpec, Matchers }

class ImperativeStringsSpec extends FlatSpec with Matchers {

  "isUniqueChars" should "tell if String has unique chars only (ASCII chars only" in {
    isUniqueChars("Aabcdefgh") shouldBe true
    isUniqueChars("aabcddefgh") shouldBe false
  }

  "checkPermutation" should "tell if two strings have the same characters in different order" in {
    permutation("Joana", "Joana") shouldBe true
    permutation("Joana ", "Joana") shouldBe false
    permutation("joana ", "Joana") shouldBe false
    permutation("Rafael", "laefRa") shouldBe true
    permutation("Rafael", "Nicolas") shouldBe false
    permutation("Amelind", "Nicolas") shouldBe false
  }

  "encodeSpace" should "replace ' ' by '%20'" ignore {
    val s = "Mr 3ohn Smith                    Mrs Joanna"
    val array = util.Arrays.copyOf(s.toCharArray, 100)

    val result = encodeSpace(array, 43)

    new String(result) shouldBe "Mr%203ohn%20Smith%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20Mrs%20Joanna"
  }

  // skipping palindrome here (see Strings)

  "isOneWay" should "tell if a string is one edit (insert, delete, replace) away from another" in {
    isOneWay("pale", "pae") shouldBe true
    isOneWay("pale", "pales") shouldBe true
    isOneWay("pale", "bale") shouldBe true
    isOneWay("pale", "pie") shouldBe false
    isOneWay("pale", "bake") shouldBe false
  }

  // skipping isRotation (1.9) here

}
