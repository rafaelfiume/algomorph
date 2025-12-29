package io.rafaelfiume.algomorph.data.interval

import io.rafaelfiume.algomorph.data.interval.*
import io.rafaelfiume.algomorph.data.interval.BoundedAlgebra.instances.given_BoundedAlgebra_Int
import io.rafaelfiume.algomorph.data.interval.DiscreteAlgebra.instances.given
import io.rafaelfiume.algomorph.data.interval.Interval.AdjacencyType.*
import io.rafaelfiume.algomorph.data.interval.Intervals
import io.rafaelfiume.algomorph.data.interval.IntervalAlgebra.syntax.*
import io.rafaelfiume.algomorph.data.interval.IntervalAlgebra.instances.given
import io.rafaelfiume.algomorph.data.interval.testkit.IntervalGens.*
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*

/**
 * The Philosophical Challenge: What Are We Proving?
 *
 * In software engineering, we are **not** demonstrating universal truths as when applying formal proofs to mathematical theorems.
 *
 * Instead, we are proving correctness against a set of specified behaviour:
 *   - **Predictability:** The components behaves as defined by specification
 *   - **Invariants Preservation:** Component state remains valid through its lifecycle
 *   - **Edge Cases:** The invariants hold for the entire range of valid inputs.
 */
class IntervalSpec extends ScalaCheckSuite:

  checkIntervalProperties("closed", Intervals.makeClosed[Int])
  checkIntervalProperties("open", Intervals.makeOpen[Int])
  checkIntervalProperties("half-open right", Intervals.makeHalfOpenRight[Int])
  checkIntervalProperties("non empty half-open right", Intervals.makeNonEmptyHalfOpenRight[Int])
  checkIntervalProperties("half-open left", Intervals.makeHalfOpenLeft[Int])

  def checkIntervalProperties[I <: Interval[Int]](name: String, factory: Factory[Int, I])(using
    alg: IntervalAlgebra[Int, I],
    num: Integral[Int],
    bound: BoundedAlgebra[Int]
  ): Unit =
    given Factory[Int, I] = factory

    val validIntervals = if alg.allowsDegenerate then intervals else strictIntervals

    val supportsAdjacentIntervals = alg.adjacencyType match
      case Meeting     => true
      case Consecutive => true
      case NonAdjacent => false

    /* Smart Constructors */

    if alg.allowsDegenerate then
      property(s"$name intervals are valid"):
        forAll(bounds) { case (start, end) =>
          val result = factory(start, end)
          result.start <= result.end
        }

    if !alg.allowsDegenerate then
      property(s"$name intervals are valid"):
        forAll(strictBounds) { case (start, end) =>
          val result = factory(start, end)
          result.start < result.end
        }

    test(s"$name intervals reject invalid bounds"):
      forAll(reversedBounds) { case (start, end) =>
        throws(classOf[IllegalArgumentException]) {
          factory(start, end)
        }
      }

    /* Degenerate intervals */

    if alg.allowsDegenerate then
      property(s"$name intervals can degenerate"):
        forAll(degenerateBounds) { case (start, end) =>
          val result = factory(start, end)
          result.isDegenerate
        }

      property(s"degenerate $name intervals are non-intersecting"):
        forAll(intervals) { a =>
          val b = factory(a.start, a.start)
          val c = factory(a.end, a.end)
          !a.intersects(b) && !a.intersects(c)
        }

      property(s"degenerate $name intervals contain only themselves"):
        forAll(strictIntervals(offsetLeft = 1, offsetRight = 1)) { a =>
          forAll(enclosingPoints(a)) { x =>
            val degenerate = factory(x, x)
            degenerate.contains(degenerate) && !degenerate.contains(a)
          }
        }

      property(s"$name intervals contain all enclosed degenerate intervals"):
        forAll(strictIntervals(offsetLeft = 1, offsetRight = 1)) { a =>
          forAll(enclosingPoints(a)) { x =>
            val degenerate = factory(x, x)
            a.contains(degenerate)
          }
        }

    /* Adjacency */

    if supportsAdjacentIntervals then
      property(s"adjacent (continuous) $name intervals never intersect"):
        forAll(adjacentIntervals) { case (a, b) =>
          a.isAdjacent(b) && !a.intersects(b)
        }

      property(s"adjacent $name intervals maintain ${alg.adjacencyType} relationship"):
        forAll(adjacentIntervals) { case (a, b) =>
          b.start == (alg.adjacencyType match
            case Meeting     => a.end
            case Consecutive => a.end + 1
            case NonAdjacent => throw new IllegalArgumentException(s"no support for adjacent intervals"))
        }

    /* Intersection */

    property(s"$name intervals intersect"):
      forAll(intersectingIntervals) { case (a, b) =>
        a.intersects(b)
      }

    property(s"intersect is reflexive iff the $name interval is not degenerate"):
      forAll(validIntervals.suchThat(!_.isDegenerate)) { a =>
        a.intersects(a)
      }

    property(s"disjoint $name intervals never intersect"):
      forAll(disjointIntervals) { case (a, b) =>
        !a.intersects(b)
      }

    property(s"intersect is symmetric for $name intervals"):
      forAll(validIntervals, validIntervals) { case (a, b) =>
        a.intersects(b) == b.intersects(a)
      }

    /* Containment */

    property(s"$name intervals contain points within their bounds"):
      forAll(strictIntervals(offsetLeft = 1, offsetRight = 1)) { a =>
        forAll(enclosingPoints(a)) { x =>
          a.contains(x)
        }
      }

    property(s"containment is reflexive for ${name} intervals"):
      forAll(strictIntervals) { a =>
        a.contains(a)
      }

    // Antisymmetry: the relation *only* goes both ways if the two elements are *identical*.
    // R(a, b) ∧ R(b, a) => a == b
    property(s"containment is antisymmetric for ${name} intervals"):
      forAll(strictIntervals, strictIntervals) { case (a, b) =>
        // a.contains(b) && b.contains(a)) => a == b
        !(a.contains(b) && b.contains(a)) || a == b
      }

    property(s"containment is transitive for ${name} intervals"):
      forAll(strictIntervals, strictIntervals, strictIntervals) { case (a, b, c) =>
        // transitivity law: (a contains b ∧ b contains c) -> (a contains c)
        // X -> Y is equivalent to ¬X ∨ Y
        !(a.contains(b) && b.contains(c)) || a.contains(c)
      }

    property(s"containing intervals intersect for ${name} intervals"):
      forAll(strictIntervals, strictIntervals) { case (a, b) =>
        // a.intersects(b) ⇒ a.intersects(b)
        !a.contains(b) || a.intersects(b)
      }

  // --------------Special Cases ---------------- //

  /* Emptiness */

  property("non-empty half-open right intervals require distinct endpoints"):
    forAll(degenerateBounds) { case (start, end) =>
      throws(classOf[IllegalArgumentException]) {
        Intervals.makeNonEmptyHalfOpenRight(start, end)
      }
    }

/**
 * We developers often say that we need to make the software we build 'deterministic'.
 *
 * I believe what we actually mean is that we need them to be more 'predictable'.
 *
 * The difference is subtle and perhaps abstract, but I think this is a good discussion, and it helps me when faced with
 * ambiguity, which is all the time.
 *
 * This might puzzle, but the software we build already are deterministic in the chaotic sense (as in non-linear deterministic
 * systems). If you find that hard to agree, just think how difficult it is to come up with **pseudo**-randomness with computers.
 * The components almost surely don't present random behaviour, but they can be extremely complex and subject to such great a
 * number of variables, to the point it become hard to predict how they behave in real-life scenarios.
 *
 * What we need then is to manage this complexity and make software less chaotic for everyone's sake.
 */
