package numbers

import scala.annotation.tailrec

object BinomialProbability:

  /**
   * Calculates the probability of a specific sequence with k successes and n-k failures. Let p = chance of success, and (1-p) be
   * the probability of failure. Then: p^k * (1-p)^(n-k).
   *
   * For example, for a sequence [Success, Success, Failure]: p * p * (1 - p).
   */
  def exactlyIndependentEventsInSequence(n: Int, k: Int, p: Double): Double =
    require(p >= 0 && p <= 1, "provide: 0 <= p <= 1")
    math.pow(p, k) * math.pow(1 - p, n - k)

  // where successes and failures can occur in any order:
  /**
   * Calculates the chances of k successes in n attempts with probability of success p: P(k) = C(n,k) * p^k * (1-p)^(n-k)
   *
   * For example, P(2) = P(exactly 2 successes in 3 attempts) = C(3, 2) * p^2 * (1-p)^(3-2) = 3 * p^2 * (1-p)
   */
  def exactlytKSuccesses(n: Int, k: Int, p: Double): Double =
    binomialCoefficient(n, k).toDouble * exactlyIndependentEventsInSequence(n, k, p)

  /**
   * Probability of at least k successes in n trials with p probability of success: P(i>=k) = Σ(from i=k to n) C(n,i) * p^i *
   * (1-p)^(n-i)
   *
   * For example, P(i>=2) = P(at least 2 successes in 3 attempts) = P(2) + P(3) = 3p^2 - 2p^3
   */
  def atLeastKSuccesses(n: Int, k: Int, p: Double): Double = (k to n).map(exactlytKSuccesses(n, _, p)).sum

  /**
   * The binomial coefficient (n chooses k).
   *
   * ===Algorithm===
   * Option 1 (simple, reads nice, wasteful):
   *
   * One possible implementation is: `factorial(n) / (factorial(n - k) * factorial(k))`. This is wasteful, even if time Θ(n),
   * since it computes 3 full factorials, leading to potentially very large intermediate values (e.g 100!) and one division and
   * one multiplication of large numbers.
   *
   * Option 2 (Pascal's Triangle): The formula `C(n, k) = C(n-1, k-1) + C(n-1, k)` is also elegant, easy to reason about and
   * straightforward to implement. It is less wasteful than Option 1, but cumbersome to implement if stack-safety is a
   * requirement.
   *
   * Option 3 (Pascal's Row Construction): Reconstruct the row n: `C(n, k)` = n * (n-1) * (n-2) * ... * (n-k+1) / k!
   *
   * ===Semantic Evaluation===
   * {{{binomialCoefficient(5, 2) C(5, 0) = 1 C(5, 1) = 1 * (5 - 1 + 1) / 1 = 5 C(5, 2) = 5 * (5 - 2 + 1) / 2 = 10}}}
   *
   * Or, if k > n /2, C(n, k) = C(n, n-k):
   * {{{
   * binomialCoefficient(5, 4) -> binomialCoefficient(5, 1)
   * }}}
   *
   * ===Complexity===
   *   - Time: Θ(min(k, n-k)) - explores binomial coefficient symmetry C(n, k) = C(n, n-k)
   *   - Space: Θ(1)
   */
  def binomialCoefficient(n: Int, k: Int): BigInt =
    require(k <= n, s"provide: 0 <= k <= n")
    val r = if k <= n / 2 then k else n - k
    @tailrec
    def loop(acc: BigInt, i: Int): BigInt =
      if i > r then acc else loop(acc * (n - i + 1) / i, i + 1)
    loop(1, 1)

  /**
   * ===Complexity===
   *   - Time: Θ(n) - n is optimal: all numbers 1 to n must be 'visited' in order to calculate factorial
   *   - Space: Θ(1)
   */
  def factorial(n: Int): BigInt =
    require(n >= 0, s"provide: n >= 0")
    @tailrec
    def loop(acc: BigInt, number: BigInt): BigInt =
      if number == 0 || number == 1 then acc
      else loop(number * acc, number - 1)
    loop(1, n)
