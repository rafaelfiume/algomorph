package data.sliding.metrics

import munit.FunSuite

class MovingMeanSpec extends FunSuite:

  test("calculates a sliding mean"):
    val avg = Sliding.mean[Int](size = 3)
    assertEquals(avg.add(3), expected = 3.0)
    assertEquals(avg.add(2), expected = 2.5)
    assertEqualsDouble(avg.add(3), expected = 2.666, delta = 0.001)
    assertEquals(avg.add(1), expected = 2d)
    assertEqualsDouble(avg.add(7), expected = 3.666, delta = 0.001)
