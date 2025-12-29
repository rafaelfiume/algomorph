package io.rafaelfiume.algomorph.numbers

import scala.annotation.tailrec

/**
 * ===Mathematical Properties===
 *
 * Isomorphism:
 *   - toBase/fromBase form a bijection between a number and valid base representations.
 *
 * Operations satisfy the Identiy Laws:
 *   - toBase . fromBase == id
 *   - fromBase . toBase == id
 *
 * Uniqueness:
 *   - Injective - For every x and y numbers, with x != y, then toBase(x) != toBase(y).
 *
 * Determinism:
 *   - Pure function - For every x and a valid radix, toBase(x, radix) == toBase(x, radix).
 *
 * ===Encoded String Length Bound===
 *
 * Given a non-negative number in the range [0 .. 2^B - 1], where B = number of bits. A number in a base expaction `b` can produce
 * b^k distinct values. We need to find the value of k number of digits to represent all values in such range. For example, if b =
 * 62, then:
 * {{{
 *   62^k > 2^B
 *    -> log₂(62^k) > log₂(2^B)
 *    -> k log₂(62) > B
 *    -> k > B / log₂(62)
 *    -> k = ceil(B / log₂(62)).
 * }}}
 *
 * For Scala Long values (signed 64-bit integers), the non-negative range is [0 .. 2^63 - 1], B = 63:
 * {{{
 *   k = ceil(63 / log(62)) ≈ ceil(10.43) = 11.
 * }}}
 *
 * ===Comparison with Standar Libraries===
 *   - For base b <= 36, yields same result as `BigInt.toString(radix)`
 *   - For base 37 <= b <= 62, extends logically with A-Z
 *
 * Base64 is not supported because:
 *   - Complexity: RFC 4648 complience requires padding (=) and line breaks
 *   - Not a binary data encoder: this is a general-purpose radix converter
 *   - Refer to java.util.Base64 if needed.
 */
object Radix:
  private val digits = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  private val digitsMap = digits.zipWithIndex.toMap

  /**
   * ===Evaluation Semantics===
   * {{{
   * toBase(11157, 62):
   *   Iter 1: n = 11157; n % 62 = 59 -> 'X'; acc = X
   *   Iter 2: n = 179; n % 62 = 55 -> 'T'; acc = TX
   *   Iter 3: n = 2; n % 62 = 2 -> '2'; acc = 2TX
   *   n = 0 -> "2TX"
   * }}}
   *
   * ===Complexity===
   *   - Time: Θ(log n)
   *   - Space: Θ(log n)
   */
  def toBase(num: BigInt, radix: Int): String =
    require(radix >= 2 && radix <= 62, "radix must be between 2 and 62")

    @tailrec
    def loop(abs: BigInt, acc: List[Char]): String =
      if abs == 0 then acc.mkString
      else // division-remainder method
        val (quot, rem) = abs /% radix
        loop(quot, digits(rem.toInt) :: acc)

    num.signum match
      case -1 => "-" + loop(-num, Nil)
      case 0  => "0"
      case 1  => loop(num, Nil)

  /**
   * ===Evaluation Semantics===
   * {{{
   *   fromBase("2tx") == (((0 * 62 + 2) * 62 + 55) * 62 + 59) == 11157
   * }}}
   *
   * ===Complexity===
   *   - Time: Θ(n) - where n = length of encoded string
   *   - Space: Θ(1)
   */
  def fromBase(num: String, radix: Int): BigInt =
    require(radix >= 2 && radix <= 62, "radix must be between 2 and 62")

    val trimmed = num.trim()
    require(trimmed.nonEmpty, "Empty input")

    val (sign, abs) = trimmed.head match
      case '+' =>
        require(trimmed.tail.nonEmpty, "Invalid number '+'")
        (1, trimmed.tail)
      case '-' =>
        require(trimmed.tail.nonEmpty, "Invalid number '-'")
        (-1, trimmed.tail)
      case _ => (1, num)

    if abs == "0" then BigInt(0)
    else // Horner's method
      sign * abs.foldLeft(BigInt(0)) { (acc, char) =>
        val digit = digitsMap(char)
        require(digit >= 0 && digit < radix, s"Invalid digit '$char' for radix $radix")
        acc * radix + digit
      }
