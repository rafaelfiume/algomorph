package numbers

import scala.annotation.tailrec

object BinaryOps:

  /**
   * Adds numbers using bitwise operations.
   *
   * ===Evaluation Semantics===
   * `add(BigInt("110", 2), BigInt("101", 2))` executes as:
   * {{{
   * - Iter 0: loop(110, 101, 0, 0, 0)
   * - Iter 1: aLsb=0; bLsb=1; sumLsb=1; carryOut=0; positionedBit=1 -> loop(11, 10, 0, 0 | 1, 1)
   * - Iter 2: aLsb=1; bLsb=0, sumLsb=1; carryOut=0; positionedBit=10 -> loop(1, 1, 0, 1 | 10, 2)
   * - Iter 3: aLsb=1; bLsb=1; sumLsb=0; carryOut=1; positionedBit=000 -> loop(0, 0, 1, 11 | 000, 3)
   * - Iter 4: aLsb=0; bLsb=0; sumLsb=1; carryOut=0; positionedBit=1000 -> loop(0, 0, 0, 11 | 1000, 4)
   * - (a == 0 && b == 0 && carry == 0): Return -> acc=1011
   * }}}
   *
   * ===Complexity===
   *   - Time: Θ(n) - where n = max(bit_length(x), bit_length(y))
   *   - Space: Θ(1)
   */
  def add(x: BigInt, y: BigInt): BigInt =
    @tailrec def loop(a: BigInt, b: BigInt, carryIn: BigInt, acc: BigInt, shift: Int): BigInt =
      if a == 0 && b == 0 && carryIn == 0 then acc
      else
        val aLsb = a & 1
        val bLsb = b & 1
        val sum = aLsb + bLsb + carryIn
        val sumLsb = sum & 1
        val carryOut = sum >> 1
        val positionedBit = sumLsb << shift
        loop(a >> 1, b >> 1, carryOut, acc | positionedBit, shift + 1)

    sign(x, y) * loop(x.abs, y.abs, 0, 0, 0)

  /**
   * Multiplies numbers using bitwise operations.
   *
   * ===Evaluation Semantics==
   * `multiply(BigInt("110", 2), BigInt("101", 2))` executes as:
   * {{{
   * - Iter 0: loop(110, 101, 0)
   * - Iter 1: (b & 1) == 1 -> loop(1100, 10, 110 + 0)
   * - Iter 2: (b & 1) == 0 -> loop(11000, 1, 110)
   * - Iter 3: (b & 1) == 1 -> loop(110000, 0, 110 + 11000)
   * - b == 0: Return -> acc=11110
   * }}}
   *
   * ===Complexity===
   *   - Time: Θ(n^2) - where n = max(bit_length(x), bit_length(y))
   *   - Space: Θ(n)
   */
  def multiply(x: BigInt, y: BigInt): BigInt =

    def bitwiseMultiply(x: BigInt, y: BigInt): BigInt =
      @tailrec
      def loop(a: BigInt, b: BigInt, acc: BigInt): BigInt =
        if b == 0 then acc
        else if (b & 1) == 1 then loop(a << 1, b >> 1, add(a, acc))
        else loop(a << 1, b >> 1, acc)

      sign(x, y) * loop(x.abs, y.abs, 0)

    // if x.bitLength < 64 || y.bitLength < 64 then bitwiseMultiply(x, y) else karatsuba(x, y) TODO
    bitwiseMultiply(x, y)

  // not a general-purpose sign function
  private def sign(x: BigInt, y: BigInt): Int = (x.signum, y.signum) match
    case (0, -1)        => -1 // critical for zero * negative number
    case (-1, 0)        => -1 // similar to above
    case (xSign, ySign) => if xSign * ySign >= 0 then 1 else -1
