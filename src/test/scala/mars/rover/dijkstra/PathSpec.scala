package mars.rover.dijkstra

import mars.rover.Grid
import mars.rover.Grid.Edge
import mars.rover.dijkstra.Path.shortestPath
import munit.Assertions.*
import munit.FunSuite

class PathSpec extends FunSuite:

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
    weights = List(Edge((0, 2), w = 4), Edge((0, 4), w = 3), Edge((1, 0), w = 3), Edge((2, 0), w = 2))
  )

  test("shortestPath finds the shortest path from one point of the grid to the other") {
    assertEquals(shortestPath(start = (0, 1), end = (0, 3), aGrid), List((0, 1), (1, 1), (1, 2), (1, 3), (0, 3)))
    assertEquals(shortestPath(start = (1, 1), end = (1, 4), aGrid), List((1, 1), (1, 2), (1, 3), (1, 4)))
    assertEquals(shortestPath(start = (2, 1), end = (2, 4), aGrid), List((2, 1), (2, 0), (2, 4)))
  }

  test("shortestPath returns an empty path when start or end nodes are not part of the grid") {
    assertEquals(shortestPath(start = (0, 0), end = (8, 8), aGrid), Nil)
    assertEquals(shortestPath(start = (8, 8), end = (0, 0), aGrid), Nil)
  }
