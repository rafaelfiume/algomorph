package cracking.ch01

object Matrix {

  def rotate[T](matrix: Array[Array[T]]): Array[Array[T]] = {
    if (matrix.length == 0) return matrix
    //    if (arr(0).length != arr(1).length) throw new IllegalArgumentException("Pass a square matrix as argument")

    val n = matrix.length
    for (layer <- 0 until n / 2) {
      val first = layer
      val last = n - 1 - layer
      for (i <- first until last) {
        println(s"fist: $first; last: $last")

        // top -> tmp
        val top = matrix(first)(i)

        // left -> top
        matrix(first)(i) = matrix(last - i)(first)

        // bottom -> left
        matrix(last - i)(first) = matrix(last)(last - i)

        // right -> bottom
        matrix(last)(last - i) = matrix(i)(last)

        // tmp -> right
        matrix(i)(last) = top
      }
    }
    matrix
  }

  def zero(matrix: Array[Array[Int]]): Unit = {
    // 1a. check if row has zeros
    var firstRowHasZero = false
    for (i <- matrix.indices) {
      if (matrix(0)(i) == 0) {
        firstRowHasZero = true
      }
    }
    // 1b. check if column has zeros
    var firstColumnHasZero = false
    for (i <- matrix.indices) {
      if (matrix(i)(0) == 0) {
        firstColumnHasZero = true
      }
    }

    // 2a. check if rows has zero from the second element
    // 2b. write in matrix[i][0] the result of the checking ^^
    // 3a. check if columns has zero from the
    // 3b. write in matrix[0][i] the result of the checking ^^
    for (r <- 1 until matrix.length) {
      for (c <- 1 until matrix.length) {
        if (matrix(r)(c) == 0) {
          matrix(r)(0) = 0
          matrix(0)(c) = 0
        }
      }
    }

    for (i <- 1 until matrix.length) {
      // 4a. iterate over matrix[i][0] and zero the rows
      if (matrix(i)(0) == 0) zeroRow(i)
      // 4a. iterate over matrix[0][i] and zero the columns
      if (matrix(0)(i) == 0) zeroColumn(i)
    }

    if (firstRowHasZero) zeroRow(0)
    if (firstColumnHasZero) zeroColumn(0)

    def zeroRow(row: Int): Unit = for (i <- matrix.indices) matrix(row)(i) = 0
    def zeroColumn(column: Int): Unit = for (i <- matrix.indices) matrix(i)(column) = 0
  }
}
