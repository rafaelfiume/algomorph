package cracking.ch01

object Question:
  def rotate(matrix: Array[Array[Int]]): Boolean =
    if matrix.length == 0 || matrix.length != matrix(0).length then return false // Not a square
    val n = matrix.length
    for layer <- 0 until n / 2 do
      val first = layer
      val last = n - 1 - layer
      for i <- first until last do
        val offset = i - first
        println(s"first: $first; last: $last")
        val top = matrix(first)(i) // save top
//        println(s"offset: $offset")
        // left -> top
        matrix(first)(i) = matrix(last - offset)(first)
        // bottom -> left
        matrix(last - offset)(first) = matrix(last)(last - offset)
        // right -> bottom
        matrix(last)(last - offset) = matrix(i)(last)
        // top -> right
        matrix(i)(last) = top // right <- saved top

    true

  def main(args: Array[String]): Unit =
    val matrix = randomMatrix(6, 6, 0, 9)
    printMatrix(matrix)
    Question.rotate(matrix)
    System.out.println()
    printMatrix(matrix)

  def randomMatrix(M: Int, N: Int, min: Int, max: Int): Array[Array[Int]] =
    val matrix: Array[Array[Int]] = Array.ofDim(M, N)
    for i <- 0 until M do for j <- 0 until N do matrix(i)(j) = randomIntInRange(min, max)
    matrix

  def randomIntInRange(min: Int, max: Int): Int = randomInt(max + 1 - min) + min

  def randomInt(n: Int): Int = (Math.random * n).toInt

  def printMatrix(matrix: Array[Array[Int]]): Unit =
    for i <- matrix.indices do
      for j <- matrix(i).indices do
        if matrix(i)(j) < 10 && matrix(i)(j) > -10 then System.out.print(" ")
        if matrix(i)(j) < 100 && matrix(i)(j) > -100 then System.out.print(" ")
        if matrix(i)(j) >= 0 then System.out.print(" ")
        System.out.print(" " + matrix(i)(j))
      System.out.println()
