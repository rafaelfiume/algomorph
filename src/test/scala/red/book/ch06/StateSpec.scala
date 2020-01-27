package red.book.ch06

import red.book.ch06.RNG.Simple
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{ FlatSpec, Matchers }

import scala.Int.MaxValue

class StateSpec extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers {

  implicit override val generatorDrivenConfig = PropertyCheckConfig(minSuccessful = 100)

  "nonNegativeInt" should "generate a random integer between 0 and Int.MaxValue" in {
    forAll { seed: Int =>
      val rng = Simple(seed)

      val result = RNG.nextNonNegativeInt(rng)._1

      result should (be >= 0 and be < MaxValue)
    }
  }

  it should "handle gracefully Int.MinValue" in {
    val minInt = new RNG {
      override def nextInt: (Int, RNG) = (Int.MinValue, Simple(-42))
    }

    val result = RNG.nextNonNegativeInt(minInt)._1

    result should (be >= 0)
  }

  "double" should "generate the next double between 0 and 1" in {
    forAll { seed: Int =>
      val rng = Simple(seed)

      val result = RNG.double(rng)._1

      result should (be >= 0d and be < 1d)
    }
  }

  "doubleViaMap" should "generate the next double between 0 and 1" in {
    forAll { seed: Int =>
      val rng = Simple(seed)

      val result = RNG._double(rng)._1

      result should (be >= 0d and be < 1d)
    }
  }

  "intDouble" should "generate a pair of an int and a double " in {
    forAll { seed: Int =>
      val rng = Simple(seed)

      val ((rInt, rDouble), _) = RNG.intDouble(rng)

      rInt should (be >= Int.MinValue and be <= Int.MaxValue)
      rDouble should (be >= 0d and be < 1d)
    }
  }

  "_intDouble (via map2)" should "generate a pair of an int and a double " in {
    forAll { seed: Int =>
      val rng = Simple(seed)

      val ((rInt, rDouble), _) = RNG._intDouble(rng)

      rInt should (be >= Int.MinValue and be <= Int.MaxValue)
      rDouble should (be >= 0d and be < 1d)
    }
  }

  "doubleInt" should "generate a pair of a double and an int " in {
    forAll { seed: Int =>
      val rng = Simple(seed)

      val ((rDouble, rInt), _) = RNG.doubleInt(rng)

      rDouble should (be >= 0d and be < 1d)
      rInt should (be >= Int.MinValue and be <= Int.MaxValue)
    }
  }

  "_doubleInt via (map2)" should "generate a pair of a double and an int " in {
    forAll { seed: Int =>
      val rng = Simple(seed)

      val ((rDouble, rInt), _) = RNG._doubleInt(rng)

      rDouble should (be >= 0d and be < 1d)
      rInt should (be >= Int.MinValue and be <= Int.MaxValue)
    }
  }

  "double3" should "generate a 3-tuple of doubles" in {
    forAll { seed: Int =>
      val rng = Simple(seed)

      val ((r1, r2, r3), _) = RNG.double3(rng)

      r1 should (be >= 0d and be < 1d)
      r2 should (be >= 0d and be < 1d)
      r3 should (be >= 0d and be < 1d)
    }
  }

  // TODO Improve this property based test
  "ints" should "generate a list of random integers" in {
    forAll { (seed: Int, size: Int) =>
      whenever (size >= 0 && size < 100) {
        val rng = Simple(seed)

        val (result, _) = RNG.ints(size)(rng)

        result.size shouldBe size
      }
    }
  }

  "_ints (via sequence)" should "generate a list of random integers" in {
    forAll { (seed: Int, size: Int) =>
      whenever (size >= 0 && size < 100) {
        val rng = Simple(seed)

        val (result, _) = RNG._ints(size)(rng)

        result.size shouldBe size
      }
    }
  }

  "nonNegativeLessThan" should "generate random integers between 0 (inclusive) and n (exclusive)" in {
    forAll { seed: Int =>
      val rng = Simple(seed)

      val (result, _) = RNG.nonNegativeLessThan(4)(rng)

      result should (be >= 0 and be < 4)
    }
  }

  "rollDie" should "return values from 1 to 6 included" in {
    forAll { seed: Int =>
      val rng = Simple(seed)

      val (result, _) = RollingDie.rollDie(rng)

      result should (be >= 1 and be <= 6)
    }
  }

  "map" should "apply a function f over a value of a state" in {
    val s = State[Int, Int](s => (s, s + 1))

    val (value, _) = s map (_.toString) run 2

    value shouldBe "2"
  }

  "flatMap" should "apply a function f over a value of state" in {
    val unit: State[Int, Int] = State(s => (s, s))
    val plus5: Int => State[Int, Int] = i => State(s => (i, s + 5))
    val mult2: Int => State[Int, Int] = i => State(s => (i, s * 2))
    val tString: Int => State[Int, String] = i => State(s => (s.toString, i))

    val (v, _) = unit flatMap plus5 flatMap { mult2 } flatMap { tString } run 2

    v shouldBe "14"
  }

//  "map2" should "lift a function to operate with two States instead of a normal value" in {
//    val between0And6 = MyRandom.s_nonNegativeLessThan(6)
//    val get: RNG => (Int, RNG) = s => (s., s)
//
//    val (v, _) = between0And6.map2(between0And6)(_+_).
//
//    v should (be >= 0 and be < 6)
//  }

}