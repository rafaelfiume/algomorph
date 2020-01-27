package red.book.ch12

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{ FlatSpec, Matchers }

class ApplicativeSpec extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers {

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
