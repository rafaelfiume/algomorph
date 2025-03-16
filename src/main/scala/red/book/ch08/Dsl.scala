package red.book.ch08

object Dsl:

  def where(guard: => Boolean)(f: => Boolean): Boolean = if guard then f else true

object MoreGens:

  val intToBooleanGen = Gen.genFn2[Boolean, Int](Gen.boolean)((b, i) => i % 2 == 0 && b)
