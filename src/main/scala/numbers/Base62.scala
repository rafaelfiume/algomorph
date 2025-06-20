package numbers // consider moving `Base62` from `numbers` to `encoding` namespace.

/**
 * Base62 is a solid alternative to hashing + collision resolution for generating compact, short and url-safe unique identifiers.
 *
 * The encoded number is deterministic and reversible, making it ideal when uniqueness and shortness are required.
 *
 * Tipical use cases are:
 *   - Url shortner
 *   - Ticketing services, where a customer receives a short, easy-to-read ticket number.
 *
 * ===Mathematical Properties===
 *
 * Isomorphic:
 *   - The encode/decode functions form an isomorphism between non-negative Long numbers and their Base62 string
 * representations
 *   - Key for data-integrity.
 *
 * Operations satisfy the Identiy Laws:
 *   - encode . decode == id
 *   - decode . encode == id
 *
 * Uniqueness:
 *   - For every x and y numbers, with x != y, then encode(x) != encode(y).
 *   - This ensures no collisions.
 *
 * Order-Preserving:
 *   - For every x and y numbers, with y > x, then encode(y) > encode(x)
 *   - Why it matters:
 *     - Efficient range queries with SQL or pagination
 *     - Enables balanced sharding
 *     - LRU caching eviction depends on key ordering to discard old entries.
 *     - Enables direct time-range queries when encoding timestamps
 */
object Base62:

  private val alphabet = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  private val base = alphabet.size

  /**
   * ===Evaluation Semantics===
   * {{{
   * encode(11157):
   *   Iter 1: n = 11157; n % 62 = 59 -> 'X'
   *   Iter 2: n = 179; n % 62 = 55 -> 'T'
   *   Iter 3: n = 2; n % 62 = 2 -> '2'
   *   Reverse "XT2" -> "2TX"
   * }}}
   *
   * ===Base 62 Encoded String Length Bound===
   *
   * Given a non-negative number in the range [0 .. 2^B - 1], where B = number of bits. Base62 can produce 62^k distinct values.
   * We need to find the value of k number of digits to represent all values in such range. Then:
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
   * ===Complexity===
   *   - Time: Θ(log n)
   *   - Space: Θ(log n)
   */
  def encode(n: Long): String =
    require(n >= 0, "provide a non-negative number")
    if n == 0 then "0"
    else LazyList.iterate(n)(_ / base).takeWhile(_ > 0).map { i => alphabet((i % base).toInt) }.reverse.mkString

  /**
   * ===Evaluation Semantics===
   * {{{
   *   decode("2tx") == (((0 * 62 + 2) * 62 + 55) * 62 + 59) == 11157L
   * }}}
   *
   * ===Complexity===
   *   - Time: Θ(n) - where n = length of encoded string
   *   - Space: Θ(1)
   */
  def decode(s: String): Long =
    s.foldLeft(0L) { (acc, c) => acc * base + alphabet.indexOf(c) }
