package numbers

import org.scalacheck.Prop.*
import munit.ScalaCheckSuite

class BinaryOpsSpec extends ScalaCheckSuite:

  property("addition is commutative"):
    forAll { (x: BigInt, y: BigInt) => BinaryOps.add(x, y) == BinaryOps.add(y, x) }

  property("addition is associative"):
    forAll { (x: BigInt, y: BigInt, z: BigInt) =>
      BinaryOps.add(BinaryOps.add(x, y), z) == BinaryOps.add(x, BinaryOps.add(y, z))
    }

  property("addition has an identity element (0)"):
    forAll { (x: BigInt) => BinaryOps.add(x, 0) == x && BinaryOps.add(0, x) == x }

  test("addition propagates carry"):
    assertEquals(
      BinaryOps.add(BigInt("111", 2), BigInt("1")),
      BigInt("1000", 2)
    )

  property("multiply is commutative"):
    forAll { (x: BigInt, y: BigInt) => BinaryOps.multiply(x, y) == BinaryOps.multiply(y, x) }

  property("multiply is associative"):
    forAll { (x: BigInt, y: BigInt, z: BigInt) =>
      BinaryOps.multiply(BinaryOps.multiply(x, y), z) == BinaryOps.multiply(x, BinaryOps.multiply(y, z))
    }

  property("multiply has an identity element (1)"):
    forAll { (x: BigInt) => BinaryOps.multiply(x, 1) == x && BinaryOps.multiply(1, x) == x }
