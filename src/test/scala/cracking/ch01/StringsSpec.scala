package cracking.ch01

import cracking.ch01.Strings._
import org.scalatest.{ FlatSpec, Matchers }

class StringsSpec extends FlatSpec with Matchers {

  "isUniqueChars" should "tell if String has unique chars only (ASCII chars only" in {
    isUniqueChars("Aabcdefgh") shouldBe true
    isUniqueChars("abcddefgh") shouldBe false
  }

  "checkPermutation" should "tell if two strings have the same characters in different order" in {
    permutation("Joana", "Joana") shouldBe true
    permutation("Joana ", "Joana") shouldBe false
    permutation("joana ", "Joana") shouldBe false
    permutation("Rafael", "laefRa") shouldBe true
    permutation("Rafael", "Nicolas") shouldBe false
    permutation("Amelind", "Nicolas") shouldBe false
  }

  "encodeSpace" should "replace ' ' by '%20'" in {
    encodeSpace("Mr 3ohn Smith                    Mrs Joanna") shouldBe "Mr%203ohn%20Smith%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20Mrs%20Joanna"
  }

  "palindrome" should "if string is a permutation of a palindrome" in {
    isPalindrome("Tact Coa") shouldBe true
    isPalindrome("Red rum, sir, is murder") shouldBe true
    isPalindrome("Was it a cat I saw") shouldBe true
    isPalindrome("Rafael") shouldBe false
  }

  "isOneWay" should "tell if a string is one edit (insert, delete, replace) away from another" in {
    isOneWay("pale", "pae") shouldBe true
    isOneWay("pale", "pales") shouldBe true
    isOneWay("pale", "bale") shouldBe true
    isOneWay("pale", "pie") shouldBe false
    isOneWay("pale", "bake") shouldBe false
  }

  "compress" should "compress a string" in {
    compress("aabcccccaaa") shouldBe "a2b1c5a3"
    compress("aabcccccaaaaaaaaaa") shouldBe "a2b1c5a10"
    compress("abc") shouldBe "abc"
  }

  // skipping rotateMatrix (1.8) here

  "isRotation" should "say is a string is a rotation of the other" in {
    isRotation("waterbottle", "erbottlewat") shouldBe true
    isRotation("abracadabra", "abracadabra") shouldBe true
    isRotation("bla", "bli") shouldBe false
    isRotation("", "") shouldBe false
  }
}
