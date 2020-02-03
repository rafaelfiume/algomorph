package mars.rover.bdf

import mars.rover.Grid
import mars.rover.bdf.Path.shortestPath
import org.scalatest.{ FlatSpec, Matchers }

class PathSpec extends FlatSpec with Matchers {

//   |-------|-------|-------|-------|-------|
//   | (0,0) | (0,1) | (0,2) | (0,3) | (0,4) |
//   |-------|-------|-------|-------|-------|
//   | (1,0) | (1,1) | (1,2) | (1,3) | (1,4) |
//   |-------|-------|-------|-------|-------|
//   | (2,0) | (2,1) | (2,2) | (2,3) | (2,4) |
//   |-------|-------|-------|-------|-------|
  private val aGrid = Grid.unweighted(rows = 3, columns = 5)

  "shortestPath" should "find the shortest path from one point of the grid to the other" in {
    shortestPath(start = (0,0), end = (2,4), aGrid) shouldBe List((0,0), (0,4), (2,4))
    shortestPath(start = (1,4), end = (0,1), aGrid) shouldBe List((1,4), (0,4), (0,0), (0,1))
    shortestPath(start = (2,0), end = (1,1), aGrid) shouldBe List((2,0), (1,0), (1,1))
    shortestPath(start = (2,4), end = (2,1), aGrid) shouldBe List((2,4), (2,0), (2,1))
    shortestPath(start = (2,2), end = (0,4), aGrid) shouldBe List((2,2), (0,2), (0,3), (0,4))
  }

//   |-------|-------|-------|
//   | (0,0) | (0,1) | (0,2) |
//   |-------|-------|-------|
//   | (1,0) | ##### | ##### |
//   |-------|-------|-------|
//   | (2,0) | (2,1) | (2,2) |
//   |-------|-------|-------|
//   | (3,0) | (3,1) | (3,2) |
//   |-------|-------|-------|
  it should "find shortest distance when there are obstacles and the target is reachable" in {
    val gridWithObstacles = Grid.unweighted(rows = 4, columns = 3, obstacles = List((1,1), (1,2)))

    val result = shortestPath(start = (2,2), end = (0,1), gridWithObstacles)

    result shouldBe List((2,2), (2,1), (3,1), (0,1))
  }

//   |-------|-------|-------|-------|
//   | ##### | (0,1) | ##### | (0,3) |
//   |-------|-------|-------|-------|
//   | ##### | (1,1) | ##### | (1,3) |
//   |-------|-------|-------|-------|
//   | ##### | (2,1) | ##### | (2,3) |
//   |-------|-------|-------|-------|
  it should "return an empty path when there's no path to the target" in {
    val obstacles = List((0,0), (1,0), (2,0), (0,2), (1,2), (2,2))
    val noWayOut = Grid.unweighted(rows = 3, columns = 4, obstacles = obstacles)

    val result = shortestPath(start = (0,1), end = (1,3), noWayOut)

    result shouldBe empty
  }

  it should "return an empty path when start or end nodes are not part of the grid" in {
    shortestPath(start = (0,0), end = (8,8), aGrid) shouldBe empty
    shortestPath(start = (8,8), end = (0,0), aGrid) shouldBe empty
  }

}
