package fp.ds.bits

object Bits {

  def add(n1: List[Int], n2: List[Int]): List[Int] = {
    def go(c: Int, n1: List[Int], n2: List[Int]): List[Int] = (n1, n2) match {
      case (Nil, Nil) => carry(c, Nil)
      case (_::_, Nil) => carry(c, n1)
      case (Nil, _::_) => carry(c, n2)
      case (x::xs, y::ys) => (x+y+c)%2 :: go((x+y+c)/2, xs, ys)
    }

    go(0, n1.reverse, n2.reverse).reverse
  }

  def carry(c: Int, number: List[Int]): List[Int] = (c, number) match {
    case (0, xs) => xs
    case (1, Nil) => List(1)
    case (1, x :: xs) => (1-x) :: carry(x, xs)
    case v => throw new RuntimeException(s"ops! Unexpected value $v")
  }
}
