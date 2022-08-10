package red.book.ch06

import munit.Assertions.*
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*
import red.book.ch06.RNG.Simple

import scala.Int.MaxValue

class StateSpec extends ScalaCheckSuite:

  property("nonNegativeInt generates a random integer between 0 and Int.MaxValue") {
    forAll { (seed: Long) =>
      val rng = Simple(seed)

      val result = RNG.nextNonNegativeInt(rng)._1

      assert(result >= 0 && result < MaxValue)
    }
  }

  test("handle gracefully Int.MinValue") {
    val minInt = new RNG:
      override def nextInt: (Int, RNG) = (Int.MinValue, Simple(-42))

    val result = RNG.nextNonNegativeInt(minInt)._1

    assert(result >= 0)
  }

  property("double generates the next double between 0 and 1") {
    forAll { (seed: Long) =>
      val rng = Simple(seed)

      val result = RNG.double(rng)._1

      assert(result >= 0d && result < 1d)
    }
  }

  property("doubleViaMap generates the next double between 0 and 1") {
    forAll { (seed: Long) =>
      val rng = Simple(seed)

      val result = RNG._double(rng)._1

      assert(result >= 0d && result < 1d)
    }
  }

  property("intDouble generates a pair of an int and a double") {
    forAll { (seed: Long) =>
      val rng = Simple(seed)

      val ((rInt, rDouble), _) = RNG.intDouble(rng)

      assert(rInt >= Int.MinValue && rInt <= Int.MaxValue)
      assert(rDouble >= 0d && rDouble < 1d)
    }
  }

  property("_intDouble (via map2) generates a pair of an int and a double") {
    forAll { (seed: Long) =>
      val rng = Simple(seed)

      val ((rInt, rDouble), _) = RNG._intDouble(rng)

      assert(rInt >= Int.MinValue && rInt <= Int.MaxValue)
      assert(rDouble >= 0d && rDouble < 1d)
    }
  }

  property("doubleInt generates a pair of a double and an int") {
    forAll { (seed: Long) =>
      val rng = Simple(seed)

      val ((rDouble, rInt), _) = RNG.doubleInt(rng)

      assert(rInt >= Int.MinValue && rInt <= Int.MaxValue)
      assert(rDouble >= 0d && rDouble < 1d)
    }
  }

  property("_doubleInt via (map2) generates a pair of a double and an int") {
    forAll { (seed: Long) =>
      val rng = Simple(seed)

      val ((rDouble, rInt), _) = RNG._doubleInt(rng)

      assert(rInt >= Int.MinValue && rInt <= Int.MaxValue)
      assert(rDouble >= 0d && rDouble < 1d)
    }
  }

  property("double3 generates a 3-tuple of doubles") {
    forAll { (seed: Long) =>
      val rng = Simple(seed)

      val ((r1, r2, r3), _) = RNG.double3(rng)

      assert(
        (r1 >= 0d && r1 < 1d) && (r2 >= 0d && r2 < 1d) && (r3 >= 0d && r3 < 1d)
      )
    }
  }

  property("ints generates a list of random integers") {
    forAll { (seed: Long, size: Int) =>
      (size >= 0 && size < 100) ==> {
        val rng = Simple(seed)

        val (result, _) = RNG.ints(size)(rng)

        result.size == size
      }
    }
  }

  property("_ints (via sequence) generates a list of random integers") {
    forAll { (seed: Long, size: Int) =>
      (size >= 0 && size < 100) ==> {
        val rng = Simple(seed)

        val (result, _) = RNG._ints(size)(rng)

        result.size == size
      }
    }
  }

  property("nonNegativeLessThan generates random integers between 0 (inclusive) and n (exclusive)") {
    forAll { (seed: Long) =>
      val rng = Simple(seed)

      val (result, _) = RNG.nonNegativeLessThan(4)(rng)

      assert(result >= 0 && result < 4)
    }
  }

  property("rollDie return values from 1 to 6 included") {
    forAll { (seed: Long) =>
      val rng = Simple(seed)

      val (result, _) = RollingDie.rollDie(rng)

      assert(result >= 1 && result <= 6)
    }
  }

  test("map apply a function f over a value of a state") {
    val s = State[Int, Int](s => (s, s + 1))

    val (value, _) = s.map(_.toString).run(2)

    assertEquals(value, "2")
  }

  test("flatMap apply a function f over a value of state") {
    val unit: State[Int, Int] = State(s => (s, s))
    val plus5: Int => State[Int, Int] = i => State(s => (i, s + 5))
    val mult2: Int => State[Int, Int] = i => State(s => (i, s * 2))
    val tString: Int => State[Int, String] = i => State(s => (s.toString, i))

    val (value, _) = unit.flatMap(plus5).flatMap { mult2 }.flatMap { tString }.run(2)

    assertEquals(value, "14")
  }

//  "map2" should "lift a function to operate with two States instead of a normal value") {
//    val between0And6 = MyRandom.s_nonNegativeLessThan(6)
//    val get: RNG => (Int, RNG) = s => (s., s)
//
//    val (v, _) = between0And6.map2(between0And6)(_+_).
//
//    v should (be >= 0 and be < 6)
//  }
