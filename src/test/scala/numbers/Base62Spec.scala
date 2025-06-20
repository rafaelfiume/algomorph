package numbers

import munit.ScalaCheckSuite
import org.scalacheck.Prop.*
import org.scalacheck.Gen.*
import org.scalacheck.ShrinkLowPriority

class Base62Spec extends ScalaCheckSuite with ShrinkLowPriority:

  test("encodes a number to base62"):
    assertEquals(Base62.encode(11157), "2tx")
    assertEquals(Base62.encode(0), "0") // minimum input
    assertEquals(Base62.encode(Long.MaxValue), "AzL8n0Y58m7") // maximum number of digits: 11

  property("encode and decode form an isomorphism"):
    forAll { (n: Long) =>
      n >= 0 ==> assert(Base62.decode(Base62.encode(n)) == n) // decode . encode = id
    }

  test("Base62 preserves uniqueness"):
    forAll(choose(0L, Long.MaxValue - 100), choose(1, 100)) { (n1, delta) =>
      val n2 = n1 + delta
      Base62.encode(n1) != Base62.encode(n2)
    }

  test("Base62 preserves ordering"):
    forAll(choose(0L, Long.MaxValue - 100), choose(1, 100)) { (n1, delta) =>
      val n2 = n1 + delta
      Base62.encode(n1) < Base62.encode(n2)
    }
