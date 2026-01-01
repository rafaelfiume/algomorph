package io.rafaelfiume.algomorph.data.interval

import io.rafaelfiume.algomorph.data.interval.{Intervals, *}
import io.rafaelfiume.algomorph.data.interval.BoundedAlgebra.instances.given_BoundedAlgebra_Int
import io.rafaelfiume.algomorph.data.interval.DiscreteAlgebra.instances.given
import io.rafaelfiume.algomorph.data.interval.IntervalAlgebra.instances.given
import io.rafaelfiume.algomorph.data.interval.IntervalAlgebra.syntax.*
import io.rafaelfiume.algomorph.data.interval.testkit.IntervalGens.*
import munit.ScalaCheckSuite
import org.scalacheck.Prop
import org.scalacheck.Prop.*

import scala.util.{Failure, Success, Try}

class IntervalSpec extends ScalaCheckSuite:

  checkIntervalProperties("closed", Intervals.makeClosed[Int])
  checkIntervalProperties("open", Intervals.makeOpen[Int])
  checkIntervalProperties("half-open right", Intervals.makeHalfOpenRight[Int])
  checkIntervalProperties("half-open left", Intervals.makeHalfOpenLeft[Int])

  def checkIntervalProperties[I <: Interval[Int]](name: String, factory: Factory[Int, I])(using
    alg: IntervalAlgebra[Int, I],
    num: Integral[Int],
    bound: BoundedAlgebra[Int]
  ): Unit =
    given Factory[Int, I] = factory

    /* Smart Constructors */

    property(s"$name intervals are valid"):
      forAll(properBounds) { case (start, end) =>
        val result = factory(start, end)
        alg.validBounds(result.start, result.end) &&
        result.start == start &&
        result.end == end
      }

    property(s"$name intervals handle degenerate bounds"):
      forAll(degenerateBounds) { case (start, end) =>
        Try(factory(start, end)) match
          case Success(interval) => alg.validBounds(start, end) && (start == end)
          case Failure(_)        => !alg.validBounds(start, end)
      }

    property(s"$name intervals reject invalid bounds"):
      forAll(reversedBounds) { case (start, end) =>
        throws(classOf[IllegalArgumentException]) {
          factory(start, end)
        }
      }

    /* Adjacency */

    property(s"adjacent (continuous) $name intervals never intersect"):
      // Vacuously true when open intervals: no adjacent intervals
      // ∀x ∈ ∅, P(x) ≡ true
      forAll(adjacentIntervals) { adjs =>
        adjs.forall { case (a, b) => a.isAdjacent(b) && !a.intersects(b) }
      }

    /* Intersection */

    property(s"$name intervals intersect"):
      forAll(intersectingIntervals) { case (a, b) =>
        a.intersects(b)
      }

    property(s"disjoint $name intervals never intersect"):
      forAll(disjointIntervals) { case (a, b) =>
        !a.intersects(b)
      }

    property(s"intersect is symmetric for $name intervals"):
      forAll(intervals, intervals) { case (a, b) =>
        a.intersects(b) == b.intersects(a)
      }

    /* Containment */

    property(s"$name intervals contain points within their bounds"):
      forAll(properIntervals(offsetLeft = 1, offsetRight = 1)) { a =>
        forAll(enclosingPoints(a)) { x =>
          a.contains(x)
        }
      }

    property(s"containment is reflexive for ${name} intervals"):
      forAll(properIntervals) { a =>
        a.contains(a)
      }

    property(s"containment is antisymmetric for ${name} intervals"):
      forAll(properIntervals, properIntervals) { case (a, b) =>
        // Antisymmetry: the relation *only* goes both ways if the two elements are *identical*.
        // R(a, b) ∧ R(b, a) => a == b
        // (a.contains(b) && b.contains(a)) => a == b
        !(a.contains(b) && b.contains(a)) || a == b
      }

    property(s"containment is transitive for ${name} intervals"):
      forAll(properIntervals, properIntervals, properIntervals) { case (a, b, c) =>
        // transitivity law: (a contains b ∧ b contains c) -> (a contains c)
        // X -> Y is equivalent to ¬X ∨ Y
        !(a.contains(b) && b.contains(c)) || a.contains(c)
      }

    property(s"containing intervals intersect for ${name} intervals"):
      forAll(properIntervals, properIntervals) { case (a, b) =>
        // a.contains(b) => a.intersects(b)
        !a.contains(b) || a.intersects(b)
      }

    property("containment preserves intersection"):
      forAll(properIntervals, properIntervals, properIntervals) { case (a, b, c) =>
        !(a.contains(b) && b.intersects(c)) || a.intersects(c)
      }

    /* Degenerate intervals */

    property(s"$name degenerate intervals behave as singletons"):
      forAll(properIntervals) { i =>
        forAll(enclosingPoints(i)) { x =>
          ifConstructible(factory(x, x)) { d =>
            d.isDegenerate &&
            i.intersects(d) && d.intersects(i) &&
            i.contains(x)
          }
        }
      }

    property(s"degenerate $name intervals contain only themselves"):
      forAll(properIntervals) { i =>
        forAll(enclosingPoints(i)) { x =>
          ifConstructible(factory(x, x)) { d => d.contains(d) && !d.contains(i) }
        }
      }

    property(s"disjoint degenerate $name intervals never intersect"):
      forAll(properIntervals) { i =>
        ifConstructible(factory(i.start, i.start) -> factory(i.end, i.end)) { case (a, b) =>
          !a.intersects(b) && !b.intersects(a)
        }
      }

    property(s"$name intervals contain all enclosed degenerate intervals"):
      forAll(properIntervals(offsetLeft = 1, offsetRight = 1)) { i =>
        forAll(enclosingPoints(i)) { x =>
          ifConstructible(factory(x, x)) { d => i.contains(d) }
        }
      }

  private def ifConstructible[T](thunk: => T)(law: T => Boolean): Boolean =
    Try(thunk).toOption.forall(law)
