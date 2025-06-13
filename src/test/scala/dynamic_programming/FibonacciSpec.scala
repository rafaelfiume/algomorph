package dynamic_programming

import dynamic_programming.Fibonacci.fib
import munit.FunSuite

class FibonacciSpec extends FunSuite:

  test("returns a fibonacy number") {
    assertEquals(fib(0).toInt, 0)
    assertEquals(fib(1).toInt, 1)
    assertEquals(fib(2).toInt, 1)
    assertEquals(fib(3).toInt, 2)
    assertEquals(fib(4).toInt, 3)
    assertEquals(fib(5).toInt, 5)
    assertEquals(fib(6).toInt, 8)
    assertEquals(fib(7).toInt, 13)
  }
