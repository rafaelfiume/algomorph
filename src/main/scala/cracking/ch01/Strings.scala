package cracking.ch01

import scala.collection.immutable.List.empty

object Strings:

  def isUniqueChars(s: String): Boolean = // only ASCII
    val sorted = s.toSeq.sorted
    sorted.zip(s.tail).map { case (a: Char, b: Char) => a != b }.forall(_ == true)

  def permutation(s1: String, s2: String): Boolean = s1.toSeq.sorted == s2.toSeq.sorted

  def encodeSpace(s: String): String =
    def doEncodeSpace(chars: List[Char]): List[Char] = chars match
      case h :: t if h == ' ' => "%20".toList ++ doEncodeSpace(t)
      case h :: t             => h :: doEncodeSpace(t)
      case Nil                => Nil

    doEncodeSpace(s.toList).mkString

  def isPalindrome(s: String): Boolean =
    def extractOddChars(toBeChecked: List[Char], odds: List[Char]): List[Char] = (toBeChecked, odds) match
      case (h :: t, acc) if acc.contains(h) => extractOddChars(t, acc.filter(_ != h))
      case (h :: t, acc)                    => extractOddChars(t, h :: acc)
      case (Nil, acc)                       => acc

    extractOddChars(s.filter(!_.isSpaceChar).map(_.toLower).toList, empty).length <= 1

  def isOneWay(s1: String, s2: String): Boolean =
    def oneEditReplace(s1: String, s2: String): Boolean =
      s1.zip(s2).foldLeft(0) { case (count, (a, b)) => if a == b then count else count + 1 } == 1

    def oneEditInsert(one: String, another: String): Boolean =
      val indexes = one.foldLeft((0, 0)) { case ((indexA, indexB), _) =>
        if one.charAt(indexA) != another.charAt(indexB) then (indexA, indexB + 1)
        else (indexA + 1, indexB + 1)
      }
      (indexes._2 - indexes._1) <= 1

    val l1 = s1.length
    val l2 = s2.length

    if l1 == l2 then oneEditReplace(s1, s2)
    else if l1 + 1 == l2 then oneEditInsert(s1, s2)
    else if l1 - 1 == l2 then oneEditInsert(s2, s1)
    else false

  def compress(s: String): String =
    type Counter = Int
    type Length = Int
    case class Compressed(length: Length, chars: List[(Char, Counter)])

    val r = s.foldRight(Compressed(0, empty[(Char, Counter)])) {
      case (char, Compressed(_, Nil)) =>
        Compressed(2, List(char -> 1))

      case (char, Compressed(length, (lastChar, counter) :: tail)) if char == lastChar =>
        val newCounter = counter + 1
        val diff = newCounter.toString.length - counter.toString.length
        Compressed(length + diff, (char, newCounter) :: tail)

      case (char, Compressed(l, st)) =>
        Compressed(l + 2, (char, 1) :: st)
    }

    if r.length >= s.length then return s

    r.chars
      .foldLeft(new StringBuilder(r.length)) { case (st, c -> count) =>
        st.appendAll(s"$c$count")
      }
      .toString()

  def isRotation(original: String, s2: String): Boolean =
    val length = original.length
    if length != s2.length || length == 0 then return false

    (original + original).contains(s2)
