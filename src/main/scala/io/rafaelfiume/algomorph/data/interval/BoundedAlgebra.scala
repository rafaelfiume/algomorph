package io.rafaelfiume.algomorph.data.interval

trait BoundedAlgebra[T]:
  def infimum: T
  def supremum: T

object BoundedAlgebra:
  object instances:
    given BoundedAlgebra[Int]:
      override def infimum: Int = Int.MinValue
      override def supremum: Int = Int.MaxValue

    given BoundedAlgebra[Long]:
      override def infimum: Long = Long.MinValue
      override def supremum: Long = Long.MaxValue
