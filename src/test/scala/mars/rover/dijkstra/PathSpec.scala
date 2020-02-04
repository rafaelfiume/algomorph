package mars.rover.dijkstra

import mars.rover.Grid
import mars.rover.Grid.Edge
import mars.rover.dijkstra.Path.shortestPath
import org.scalatest.{ FlatSpec, Matchers }

class PathSpec extends FlatSpec with Matchers {

  //   |-------|-------|-------|-------|-------|
  //   | (0,0) | (0,1) | **4** | (0,3) | **3** |
  //   |-------|-------|-------|-------|-------|
  //   | **3** | (1,1) | (1,2) | (1,3) | (1,4) |
  //   |-------|-------|-------|-------|-------|
  //   | **2** | (2,1) | (2,2) | (2,3) | (2,4) |
  //   |-------|-------|-------|-------|-------|
  private val aGrid = Grid.weighted(
    rows = 3,
    columns = 5,
    weights = List(Edge((0,2), w=4), Edge((0,4), w=3), Edge((1,0), w=3), Edge((2,0), w=2))
  )

  "shortestPath" should "find the shortest path from one point of the grid to the other" in {
    shortestPath(start = (0,1), end = (0,3), aGrid) shouldBe List((0,1), (1,1), (1,2), (1,3), (0,3))
    shortestPath(start = (1,1), end = (1,4), aGrid) shouldBe List((1,1), (1,2), (1,3), (1,4))
    shortestPath(start = (2,1), end = (2,4), aGrid) shouldBe List((2,1), (2,0), (2,4))
  }

  it should "return an empty path when start or end nodes are not part of the grid" in {
    shortestPath(start = (0,0), end = (8,8), aGrid) shouldBe empty
    shortestPath(start = (8,8), end = (0,0), aGrid) shouldBe empty
  }
}
