package cracking.ch01

import java.util

import cracking.ch01.ImperativeStrings.*
import munit.Assertions.*
import munit.FunSuite
import org.scalacheck.Prop.forAll

class ImperativeStringsSpec extends FunSuite:

  test("isUniqueChars tells if String has unique chars only (ASCII chars only") {
    assert(isUniqueChars("Aabcdefgh"))
    assert(!isUniqueChars("aabcddefgh"))
  }

  test("checkPermutation tells if two strings have the same characters in different order") {
    assert(permutation("Joana", "Joana"))
    assert(permutation("Rafael", "laefRa"))
    assert(!permutation("Joana ", "Joana"))
    assert(!permutation("joana ", "Joana"))
    assert(!permutation("Rafael", "Nicolas"))
    assert(!permutation("Amelind", "Nicolas"))
  }

  test("encodeSpace replaces ' ' by '%20'".ignore) { // TODO
    val s = "Mr 3ohn Smith                    Mrs Joanna"
    val array = util.Arrays.copyOf(s.toCharArray, 100)

    val result = encodeSpace(array, 43)

    assertEquals(new String(result), "Mr%203ohn%20Smith%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20Mrs%20Joanna")
  }

  // skipping palindrome here (see Strings)

  test("isOneWay tells if a string is one edit (insert, delete, replace) away from another") {
    assert(isOneWay("pale", "pae"))
    assert(isOneWay("pale", "pales"))
    assert(isOneWay("pale", "bale"))
    assert(!isOneWay("pale", "pie"))
    assert(!isOneWay("pale", "bake"))
  }

// skipping isRotation (1.9) here
