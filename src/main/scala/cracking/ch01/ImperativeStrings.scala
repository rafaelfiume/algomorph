package cracking.ch01

import scala.collection.mutable

object ImperativeStrings {

  def isUniqueChars(s: String): Boolean = { // only ASCII
    if (s.length > 128) return false

    val chars_set = new Array[Boolean](128)
    for (i <- 0 until s.length) {
      val charVal = s.charAt(i)
      if (chars_set(charVal)) {
        return false
      } else {
        chars_set(charVal) = true
      }
    }
    true
  }

  // we could alternatively implement this sorting the string, which is much cleaner
  // S = O(N) // R = O(N)
  def permutation(s1: String, s2: String): Boolean = {
    if (s1 equals s2) return true
    if (s1.length != s2.length) return false

    val dic = mutable.HashMap.empty[Char, Integer]
    s1 foreach { c =>
      dic.updateWith(c) {
        case Some(count) => Some(count + 1)
        case None => Some(1)
      }
    }

    var equal = true
    s2.foreach { c =>
      dic.updateWith(c) {
        case Some(count) =>
          val counter = count - 1
          if (counter < 0) equal = false
          Some(counter)
        case None =>
          equal = false
          None
      }
    }
    equal
  }

  def encodeSpace(chars: Array[Char], sSize: Int): Array[Char] = {
    def countSpacesIn(array: Array[Char]): Int = array.foldLeft(0) { (size, c) => if (c == ' ') size + 1 else size }

    val numOfSpaces = countSpacesIn(chars)
    var trueIndex = sSize - 1 + (numOfSpaces * 2)

    if (trueIndex > sSize + 1) chars(trueIndex + 1) = '\u0000'
    for (i <- sSize - 1 to 0 by -1) {
      if (chars(i) == ' ') {
        chars(trueIndex) = '0'
        chars(trueIndex - 1) = '2'
        chars(trueIndex - 2) = '%'
        trueIndex = trueIndex - 3
      } else {
        chars(trueIndex) = chars(i)
        trueIndex = trueIndex - 1
      }
    }
    chars
  }

  def isOneWay(s1: String, s2: String): Boolean = {
    def oneEditReplace(one: String, another: String): Boolean = {
      var isDifferent = false
      for (i <- 0 until one.length) {
        if (one.charAt(i) != another.charAt(i)) {
          if (isDifferent) return false
          isDifferent = true
        }
      }
      true
    }

    def oneEditInsert(one: String, another: String): Boolean = {
      var oneIndex = 0
      var anotherIndex = 0
      for (_ <- 0 until one.length) {
        if (one.charAt(oneIndex) == another.charAt(anotherIndex)) {
          oneIndex = oneIndex + 1
          anotherIndex = anotherIndex + 1
        } else {
          if (oneIndex != anotherIndex) return false
          anotherIndex = anotherIndex + 1
        }
      }
      true
    }

    val l1 = s1.length
    val l2 = s2.length

    if (l1 == l2) oneEditReplace(s1, s2)
    else if (l1 + 1 == l2) oneEditInsert(s1, s2)
    else if (l1 - 1 == l2) oneEditInsert(s2, s1)
    else false
  }
}
