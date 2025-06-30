package data.sliding.metrics

import munit.FunSuite

class SlidingArithmeticSpec extends FunSuite:

  test("computes a sliding min"):
    val min = Sliding.min[Int](size = 3)
    assertEquals(min.add(3), expected = 3)
    assertEquals(min.add(2), expected = 2)
    assertEquals(min.add(3), expected = 2)
    assertEquals(min.add(1), expected = 1)
    assertEquals(min.add(7), expected = 1)

  test("computes a sliding max"):
    val max = Sliding.max[Int](size = 3)
    assertEquals(max.add(3), expected = 3)
    assertEquals(max.add(2), expected = 3)
    assertEquals(max.add(3), expected = 3)
    assertEquals(max.add(1), expected = 3)
    assertEquals(max.add(7), expected = 7)
