package cracking.ch01

import cracking.ch01.Matrix._
import org.scalatest.{ FlatSpec, Matchers }

class MatrixSpec extends FlatSpec with Matchers {

  //  Current Matrix:               90d Rotated Matrix:
  //  1 | 2 | 3 | 4                 3 | 9 | 5 | 1
  //  5 | 6 | 7 | 8                 4 | 0 | 6 | 2
  //  9 | 0 | 1 | 2                 5 | 1 | 7 | 3
  //  3 | 4 | 5 | 6                 6 | 2 | 8 | 4
  "rotateMatrix" should "rotate a square matrix 90 degrees" ignore new MatrixSpecContext { // TODO Coming soon
    val r1 = rotate(array4x4)

    r1 shouldBe Array(
      Array(3, 9, 5, 1),
      Array(4, 0, 6, 2),
      Array(5, 1, 7, 3),
      Array(6, 2, 8, 4)
    )

    val r2 = rotate(Array(
      Array(5, 6, 6, 4, 4, 0),
      Array(4, 1, 6, 8, 9, 4),
      Array(1, 1, 0, 2, 0, 8),
      Array(9, 9, 4, 2, 1, 2),
      Array(1, 4, 3, 4, 6, 3),
      Array(8, 8, 7, 3, 5, 2)
    ))
    r2 shouldBe Array(
      Array(8, 1, 9, 1, 4, 5),
      Array(8, 4, 9, 1, 1, 6),
      Array(7, 3, 4, 0, 6, 6),
      Array(3, 4, 2, 2, 8, 4),
      Array(5, 6, 1, 0, 9, 4),
      Array(2, 3, 2, 8, 4, 0)
    )
  }

  "zeroMatrix" should "fill row and column of a matrix MxN with zeros if an element is 0" in new MatrixSpecContext {
    zero(array4x4)
    array4x4 shouldBe Array(
      Array(1, 0, 3, 4),
      Array(5, 0, 7, 8),
      Array(0, 0, 0, 0),
      Array(3, 0, 5, 6)
    )

    val array6x6 = Array(
      Array(1, 2, 0, 4, 1, 5),
      Array(5, 6, 7, 8, 3, 4),
      Array(9, 2, 1, 2, 2, 8),
      Array(3, 4, 5, 6, 7, 6),
      Array(3, 4, 5, 6, 9, 2),
      Array(0, 4, 5, 6, 9, 1)
    )
    zero(array6x6)
    array6x6 shouldBe Array(
      Array(0, 0, 0, 0, 0, 0),
      Array(0, 6, 0, 8, 3, 4),
      Array(0, 2, 0, 2, 2, 8),
      Array(0, 4, 0, 6, 7, 6),
      Array(0, 4, 0, 6, 9, 2),
      Array(0, 0, 0, 0, 0, 0)
    )

    val anotherArray4x4 = Array(
      Array(0, 2, 3, 4),
      Array(5, 6, 7, 8),
      Array(9, 1, 1, 2),
      Array(3, 4, 5, 6)
    )
    zero(anotherArray4x4)
    anotherArray4x4 shouldBe Array(
      Array(0, 0, 0, 0),
      Array(0, 6, 7, 8),
      Array(0, 1, 1, 2),
      Array(0, 4, 5, 6)
    )
  }

  it should "not change matrix MxN if there's no element whose value is 0" in new MatrixSpecContext {
    val noZeros = Array(
      Array(1, 2, 3, 4),
      Array(5, 6, 7, 8),
      Array(9, 1, 1, 2),
      Array(3, 4, 5, 6)
    )
    zero(noZeros)
    noZeros shouldBe Array(
      Array(1, 2, 3, 4),
      Array(5, 6, 7, 8),
      Array(9, 1, 1, 2),
      Array(3, 4, 5, 6)
    )
  }
}

class MatrixSpecContext {
   val array4x4 = Array(
    Array(1, 2, 3, 4),
    Array(5, 6, 7, 8),
    Array(9, 0, 1, 2),
    Array(3, 4, 5, 6)
  )
}