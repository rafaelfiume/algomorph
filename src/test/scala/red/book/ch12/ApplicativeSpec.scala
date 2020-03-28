package red.book.ch12

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks


class ApplicativeSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  "map2BasedOnProductAndMap" should "lift a function and apply its two arguments to that function" in {
    val F = ApplicativeInstances.optionApplicative

    forAll { (a: Option[Int], b: Option[Int], f: (Int, Int) => Int)  =>
      F.map2(a, b)(f) shouldBe a.flatMap { va =>
        b.map { vb =>
          f(va, vb)
        }
      }
    }
  }
}
