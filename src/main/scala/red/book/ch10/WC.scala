package red.book.ch10

import red.book.ch10.Monoids.foldMapV

// Ex. 10.10
sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lstub: String, words: Int, rstub: String) extends WC

object WC {

  val wcMonoid = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = {
      val r = (a1, a2) match {
        case (Stub(cl), Stub(cr)) => Stub(cl ++ cr)
        case (Stub(cl), Part(l, n, r)) => Part(cl ++ l, n, r)
        case (Part(l, n, r), Stub(cr)) => Part(l, n, r ++ cr)
        case (Part(l1, n1, r1), Part(l2, n2, r2)) => Part(l1, n1 + n2 + (if((r1 ++ l2).contains(" ")) 1 else 0), r2)
      }
      r
    }

    override def zero: WC = Stub("")
  }

  def countWords(s: String): Int = {
    def unstash(s: String) = if (s.trim.isEmpty) 0 else 1
    def toWC(c: Char) = if (c == ' ') Part(" ", 0, " ") else Stub(c.toString)

    foldMapV(s, wcMonoid)(toWC) match {
      case Part(l, n, r) => unstash(l) + n + unstash(r)
      case v => throw new RuntimeException(s"ops! Unexpected value $v")
    }
  }
}