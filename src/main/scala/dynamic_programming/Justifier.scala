package dynamic_programming

import scala.annotation.tailrec

object Justifier:

  def justify(text: String, width: Int): List[String] =
    val lines = computeLineBreaks(text, width)
    format(lines, width)

  /*
   * Computes the optimal words composing each `text` line based on `maxWidth`.
   *
   * ===Algorithm==
   *   - Pre-compute cumulative word lengths for O(1) access time ruding badness calculation
   *   - Define:
   *     - dp[i] = minimum total badness for first i words
   *     - parents[i] = optimal start index of last line for the first i words.
   *   - Reconstruct lines backwards using parents pointers.
   *
   * ===Complexity===
   *   - Time: Θ(n^2) - where n = number of words in `text`
   *   - Space: Θ(n)
   */
  private def computeLineBreaks(text: String, width: Int): List[Array[String]] =
    require(width > 0, "maxWidth must be positive")
    val words = text.trim.split("\\s+")
    require(words.forall(_.length <= width), s"words must be smaller than width=$width")

    def optimalLineBreaks(words: Array[String], width: Int): (Array[Int], Array[Int]) =
      val n = words.length

      val cumulativeLength = Array.ofDim[Int](n + 1)
      cumulativeLength(0) = 0
      for i <- 1 to n do cumulativeLength(i) = cumulativeLength(i - 1) + words(i - 1).length

      val dp = Array.fill[Int](n + 1)(Int.MaxValue)
      dp(0) = 0 // base case: no words == no cost
      val parents = Array.ofDim[Int](n + 1)

      for i <- 1 to n do
        for j <- 0 until i do
          val gaps = i - j - 1
          val lineWidth = cumulativeLength(i) - cumulativeLength(j) + gaps
          val extra = width - lineWidth

          if extra >= 0 then
            val badness =
              if i == n then 0 // last line - left justified - no cost
              else if gaps == 0 then extra * extra // one line word
              else extra * extra * extra // multi-word line

            val cost = dp(j) + badness
            if dp(i) > cost then
              dp(i) = cost
              parents(i) = j
      (dp, parents)

    def reconstructLines(words: Array[String], parents: Array[Int]): List[Array[String]] =
      @tailrec
      def loop(text: List[Array[String]], i: Int): List[Array[String]] =
        val j = parents(i)
        val line = words.slice(j, i)
        val newText = line :: text
        if j == 0 then newText else loop(newText, j)
      loop(List.empty[Array[String]], words.length)

    val (_, parents) = optimalLineBreaks(words, width)
    reconstructLines(words, parents)

  /*
   * Rules:
   *   - Left-justify single-word and last line
   *   - Fully-justify multi-words line.
   *
   * ===Complexity===
   *   - Time: Θ(m*n) - where m = largest line length, and n = number of lines
   *   - Space: Θ(g) - where g = largest line length
   */
  private def format(lines: List[Array[String]], width: Int): List[String] =
    def justifyLine(words: Array[String], isLastLine: Boolean): String =
      if words.length == 0 then " " * width
      else if words.length == 1 || isLastLine then
        val line = words.mkString(" ")
        line + " " * (width - line.length)
      else
        val gaps = words.length - 1
        val cumulativeLength = words.map(_.length).sum
        val extra = width - cumulativeLength
        val extraPerGap = extra / gaps
        val remaining = extra % gaps
        val spaces = Array.fill(gaps)(extraPerGap)
        for i <- 0 until remaining do spaces(i) += 1
        val builder = StringBuilder()
        words.indices.foreach { i =>
          builder.append(words(i))
          if i < gaps then builder.append(" " * spaces(i))
        }
        builder.toString()

    def isLastLine(i: Int) = i == lines.length - 1
    lines.zipWithIndex.map { (words, i) => justifyLine(words, isLastLine(i)) }

object PlayIt:
  def main(args: Array[String]): Unit =
    val text = "The house is on fire! We found this hotwheels on the floor, and its name is Bombshaker 🔥"
    println(Justifier.justify(text, 10).mkString("\n"))

//println(s"(i, j)=($i, $j); lineWidth=$lineWidth; extra=$extra; badness=$badness")
//println(s"cost = dp(j) + badness = $cost")
//println(s"dp($i) = ${dp(i)}; parent($i)=$j")
