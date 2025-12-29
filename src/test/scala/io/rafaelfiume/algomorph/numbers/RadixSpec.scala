package io.rafaelfiume.algomorph.numbers

import munit.ScalaCheckSuite
import org.scalacheck.Prop.*
import org.scalacheck.Gen.*
import org.scalacheck.Gen
import org.scalacheck.ShrinkLowPriority

class RadixSpec extends ScalaCheckSuite with ShrinkLowPriority:

  test("converts a number to a given base expansion"):
    assertEquals(Radix.toBase(8, 2), "1000")

  property("toBase and fromBase form an isomorphism"):
    forAll(choose(0L, Long.MaxValue), radixes()) { (num, radix) =>
      assert(Radix.fromBase(Radix.toBase(num, radix), radix) == num) // Encoding/decoding (of valid string representation)
    }

  property("radix is injective (preserves uniqueness)"):
    forAll(choose(Long.MinValue, Long.MaxValue), radixes()) { (num, radix) =>
      (num < Long.MaxValue) ==> {
        Radix.toBase(num, radix) != Radix.toBase(num + 1, radix)
      }
    }

  property("toBase is deterministic"):
    forAll(choose(Long.MinValue, Long.MaxValue), radixes()) { (num, radix) =>
      (num < Long.MaxValue) ==> {
        assertEquals(
          Radix.toBase(num, radix),
          Radix.toBase(num, radix)
        )
      }
    }

  /*
   * A note on isomorphism and toBase/fromBase zero handling:
   *
   *  - Isomorphism requires value preservation: `fromBase(toBase(n, radix), radix) == n`.
   *  - It does _not_ require string representation preservation: `toBase(fromBase(s, radix), radix) = s`.
   *  - toBase/fromBase always preserves value, but not necessarily string representation: `-0 == 0`.
   *
   * I.e. fromBase accepts various representations of zero. toBase always emit the canonical form `"0"`.
   */
  test("handles zero properly"):
    val zero = BigInt(0)
    val radix = radixes().sample.get
    assertEquals(Radix.fromBase("0", radix), expected = zero)
    assertEquals(Radix.fromBase("+0", radix), expected = zero)
    assertEquals(Radix.fromBase("-0", radix), expected = zero)
    assertEquals(Radix.fromBase("000", radix), expected = zero)
    // canonical string representation
    assertEquals(Radix.toBase(zero, radix), expected = "0")

  test("fromBase handles trailing zeros"):
    assertEquals(Radix.fromBase("0010", 2), expected = BigInt(2))

  /*
   * Base62 is a solid alternative to hashing + collision resolution for generating compact,
   * short and url-safe unique identifiers.
   * 
   * The encoded number is deterministic and reversible, making it ideal when uniqueness and shortness are required.
   * 
   * Tipical use cases are:
   *   - Url shortner
   *   - Ticketing services, where a customer receives a short, easy-to-read ticket number.
   */
  List(
    // format: off
    (11157L,        62, "2TX"),
    (0L,            62, "0"),
    (Long.MaxValue, 62, "aZl8N0y58M7")
    // format: on
  ).foreach { case (number, radix, expected) =>
    test("encodes a number to base62"):
      assertEquals(Radix.toBase(number, radix), expected)
  }

  private def radixes(): Gen[Int] = Gen.choose(2, 62)
