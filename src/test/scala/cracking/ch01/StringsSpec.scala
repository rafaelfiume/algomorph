package cracking.ch01

import cracking.ch01.Strings.*
import munit.Assertions.*
import munit.FunSuite

class StringsSpec extends FunSuite:

  test("check if String contains only unique ASCII chars") {
    assert(isUniqueChars("Aabcdefgh"))
    assert(!isUniqueChars("abcddefgh"))
  }

  test("check if two strings have the same characters in different order") {
    assert(permutation("Joana", "Joana"))
    assert(permutation("Rafael", "laefRa"))
    assert(!permutation("Joana ", "Joana"))
    assert(!permutation("joana ", "Joana"))
    assert(!permutation("Rafael", "Nicolas"))
    assert(!permutation("Amelind", "Nicolas"))
  }

  test("replace ' ' by '%20'") {
    assertEquals(
      encodeSpace("Mr 3ohn Smith                    Mrs Joanna"),
      "Mr%203ohn%20Smith%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20Mrs%20Joanna"
    )
  }

  test("check if string is a permutation of a palindrome") {
    assert(isPalindrome("Tact Coa"))
    assert(isPalindrome("Red rum, sir, is murder"))
    assert(isPalindrome("Was it a cat I saw"))
    assert(!isPalindrome("Rafael"))
  }

  test("check if a string is one edit (insert, delete, replace) away from another") {
    assert(isOneWay("pale", "pae"))
    assert(isOneWay("pale", "pales"))
    assert(isOneWay("pale", "bale"))
    assert(!isOneWay("pale", "pie"))
    assert(!isOneWay("pale", "bake"))
  }

  test("compress a string") {
    assertEquals(compress("aabcccccaaa"), "a2b1c5a3")
    assertEquals(compress("aabcccccaaaaaaaaaa"), "a2b1c5a10")
    assertEquals(compress("abc"), "abc")
  }

  // skipping rotateMatrix (1.8) here

  test("check whether one string is a rotation of another") {
    assert(isRotation("waterbottle", "erbottlewat"))
    assert(isRotation("abracadabra", "abracadabra"))
    assert(!isRotation("bla", "bli"))
    assert(!isRotation("", ""))
  }
