package io.rafaelfiume.algomorph.data.sliding.metrics

import munit.ScalaCheckSuite
import org.scalacheck.Prop.*
import org.scalacheck.Gen.*

class SlidingStatisticsSpec extends ScalaCheckSuite:

  test("computes a sliding mean"):
    val avg = Sliding.mean[Int](size = 3)
    assertEquals(avg.add(3), expected = 3.0)
    assertEquals(avg.add(2), expected = 2.5)
    assertEqualsDouble(avg.add(3), expected = 2.666, delta = 0.001)
    assertEquals(avg.add(1), expected = 2d)
    assertEqualsDouble(avg.add(7), expected = 3.666, delta = 0.001)

  test("sliding moments describes the distribution and shape of a dataset"):
    val moments = Sliding.moments[Int](size = 3)
    // This is handy: https://www.mathsisfun.com/data/standard-deviation-calculator.html
    val input = List(-5, 1, 8, 7, 2)

    val result = input.map(moments.add).last

    assertEquals(result.mean, expected = 5.666666666666667)
    assertEquals(result.variance, expected = 6.888888888888886)
    assertEquals(result.standardDeviation, expected = 2.62466929133727)

  test(s"sliding mean and sliding moments computes the same mean"):
    forAll(nonEmptyListOf(double)) { input =>
      val slidingMean = Sliding.mean[Double](size = 3)
      val slidingMoments = Sliding.moments[Double](size = 3)

      val mean = input.map(slidingMean.add).last
      val moment = input.map(slidingMoments.add).last

      assertEquals(mean, moment.mean)
    }
