package red.book.ch01

import red.book.ch01.MyModule.fib
import munit.Assertions.*
import munit.FunSuite

class MyModuleSpec extends FunSuite:

  test("fib retrieves the nth number in a fibonacy sequence (0,1,1,2,3,5,8..)") {
//    assertEquals(fib(-1), 0) // invalid
//    assertEquals(fib(0), 0) // invalid
    assertEquals(fib(1), 0)
    assertEquals(fib(2), 1)
    assertEquals(fib(3), 1)
    assertEquals(fib(4), 2)
    assertEquals(fib(5), 3)
    assertEquals(fib(6), 5)
    assertEquals(fib(7), 8)
  }
