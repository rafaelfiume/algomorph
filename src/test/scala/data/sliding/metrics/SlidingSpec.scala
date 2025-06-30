package data.sliding.metrics

import munit.ScalaCheckSuite
import org.scalacheck.Prop.*
import org.scalacheck.Gen.*

class SlidingSpec extends ScalaCheckSuite:

  // === Statistics ===

  test("computes a sliding mean"):
    val avg = Sliding.mean[Int](size = 3)
    assertEquals(avg.add(3), expected = 3.0)
    assertEquals(avg.add(2), expected = 2.5)
    assertEqualsDouble(avg.add(3), expected = 2.666, delta = 0.001)
    assertEquals(avg.add(1), expected = 2d)
    assertEqualsDouble(avg.add(7), expected = 3.666, delta = 0.001)

  test("sliding moments describes the distribution and shape of a dataset"):
    val moments = Sliding.moments[Int](size = 5)
    val input = List(-5, 1, 8, 7, 2)

    val result = input.map(moments.add).last

    assertEquals(result.mean, expected = 2.6)
    assertEquals(result.variance, expected = 21.84)
    assertEquals(result.standardDeviation, expected = 4.673328578219169)

  test(s"sliding mean and sliding moments computes the same mean"):
    forAll(nonEmptyListOf(double)) { input =>
      val slidingMean = Sliding.mean[Double](5)
      val slidingMoments = Sliding.moments[Double](5)

      val mean = input.map(slidingMean.add).last
      val moment = input.map(slidingMoments.add).last

      assertEquals(mean, moment.mean)
    }

  // === Arithmetic ===

  test("computes a sliding max"):
    val max = Sliding.max[Int]()
    assertEquals(max.add(3), expected = 3)
    assertEquals(max.add(2), expected = 3)
    assertEquals(max.add(3), expected = 3)
    assertEquals(max.add(1), expected = 3)
    assertEquals(max.add(7), expected = 7)

  test("computes a sliding min"):
    val min = Sliding.min[Int]()
    assertEquals(min.add(3), expected = 3)
    assertEquals(min.add(2), expected = 2)
    assertEquals(min.add(3), expected = 2)
    assertEquals(min.add(1), expected = 1)
    assertEquals(min.add(7), expected = 1)
