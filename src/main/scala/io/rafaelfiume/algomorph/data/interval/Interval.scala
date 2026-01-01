package io.rafaelfiume.algomorph.data.interval

/**
 * A discrete interval between two ordered values.
 */
sealed trait Interval[T]:
  // intrinsic Interval properties are represented as vals/methods in the trait
  val start: T
  val end: T
  def isDegenerate: Boolean = false
  def isProper: Boolean = true

  override def equals(that: Any): Boolean = that match
    case other: Interval[?] if this.getClass == other.getClass =>
      (this.start == other.start) && (this.end == other.end)
    case _ => false

  override def hashCode(): Int = (start, end).##

object Interval:

  /**
   * Encodes which adjacency relationships are possible for a given interval type.
   */
  enum AdjacencyType:
    case Meeting // half-open right: [4, 6) & [6, 8) // half-open left: (4, 6] & (6, 8]
    case Consecutive //      closed: [4, 6] & [7, 8]
    case NonAdjacent //        open: (4, 6) & (6, 8)

  /**
   * A closed interval [start, end].
   */
  sealed abstract case class Closed[T](start: T, end: T) extends Interval[T]:
    override def isDegenerate: Boolean = start == end
    override def isProper: Boolean = start != end
    override def toString(): String = s"[$start, $end]"

  /**
   * An open interval (start, end).
   */
  sealed abstract case class Open[T](start: T, end: T) extends Interval[T]:
    override def toString(): String = s"($start, $end)"

  /**
   * A half-open right interval [start, end).
   */
  sealed abstract case class HalfOpenRight[T](start: T, end: T) extends Interval[T]:
    override def toString(): String = s"[$start, $end)"

  /**
   * A half-open left interval (start, end].
   */
  sealed abstract case class HalfOpenLeft[T](start: T, end: T) extends Interval[T]:
    override def toString(): String = s"($start, $end]"

object Intervals:
  import io.rafaelfiume.algomorph.data.interval.Interval.*
  import Ordering.Implicits.*

  def makeClosed[T: Ordering](start: T, end: T): Closed[T] =
    require(start <= end, s"invariant error: start=$start must be <= end=$end")
    new Closed(start, end) {}

  def makeOpen[T: Ordering](start: T, end: T): Open[T] =
    require(start < end, s"invariant error: start=$start must be < end=$end")
    new Open(start, end) {}

  def makeHalfOpenRight[T: Ordering](start: T, end: T): HalfOpenRight[T] =
    require(start < end, s"invariant error: start=$start must be < end=$end")
    new HalfOpenRight(start, end) {}

  def makeHalfOpenLeft[T: Ordering](start: T, end: T): HalfOpenLeft[T] =
    require(start < end, s"invariant error: start=$start must be < end=$end")
    new HalfOpenLeft(start, end) {}
