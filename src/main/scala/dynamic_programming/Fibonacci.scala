package dynamic_programming

import scala.collection.*
import scala.collection.mutable.ArrayBuffer

object Fibonacci:
  def fib(n: Int): BigInt =
    def fib0(n: Int, memo: ArrayBuffer[BigInt]): BigInt =
      require(n >= 0, s"invalid negative number $n")
      if memo(n) != -1 then memo(n)
      else
        val r: BigInt = n match
          case 0 => 0
          case 1 => 1
          case n => fib0(n - 2, memo) + fib0(n - 1, memo)
        memo(n) = r
        r

    fib0(n, memo = ArrayBuffer.fill[BigInt](n + 1)(-1))

object PlayFib:
  import dynamic_programming.Fibonacci.*

  def main(args: Array[String]): Unit =
    for i <- 0 to 300 do
      val r = fib(i)
      println(s"fib($i) = $r")
