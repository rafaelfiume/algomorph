package red.book.ch12

import munit.Assertions.*
import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

class ApplicativeSpec extends ScalaCheckSuite:

  test("lift a function and apply its two arguments to that function") {
    val F = ApplicativeInstances.optionApplicative
    forAll { (a: Option[Int], b: Option[Int], f: (Int, Int) => Int) =>
      assertEquals(
        F.map2(a, b)(f),
        a.flatMap { va => b.map { vb => f(va, vb) } }
      )
    }
  }
