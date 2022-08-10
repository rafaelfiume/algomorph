package mars.rover.bdf

import mars.rover.Grid
import mars.rover.bdf.Path.shortestPath
import munit.Assertions.*
import munit.FunSuite

class PathSpec extends FunSuite:

//   |-------|-------|-------|-------|-------|
//   | (0,0) | (0,1) | (0,2) | (0,3) | (0,4) |
//   |-------|-------|-------|-------|-------|
//   | (1,0) | (1,1) | (1,2) | (1,3) | (1,4) |
//   |-------|-------|-------|-------|-------|
//   | (2,0) | (2,1) | (2,2) | (2,3) | (2,4) |
//   |-------|-------|-------|-------|-------|
  private val aGrid = Grid.unweighted(rows = 3, columns = 5)

  test("shortestPath finds the shortest path from one point of the grid to the other") {
    assertEquals(shortestPath(start = (0, 0), end = (2, 4), aGrid), List((0, 0), (0, 4), (2, 4)))
    assertEquals(shortestPath(start = (1, 4), end = (0, 1), aGrid), List((1, 4), (0, 4), (0, 0), (0, 1)))
    assertEquals(shortestPath(start = (2, 0), end = (1, 1), aGrid), List((2, 0), (1, 0), (1, 1)))
    assertEquals(shortestPath(start = (2, 4), end = (2, 1), aGrid), List((2, 4), (2, 0), (2, 1)))
    assertEquals(shortestPath(start = (2, 2), end = (0, 4), aGrid), List((2, 2), (0, 2), (0, 3), (0, 4)))
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
  test("find shortest distance when there are obstacles and the target is reachable") {
    val gridWithObstacles = Grid.unweighted(rows = 4, columns = 3, obstacles = List((1, 1), (1, 2)))

    val result = shortestPath(start = (2, 2), end = (0, 1), gridWithObstacles)

    assertEquals(result, List((2, 2), (2, 1), (3, 1), (0, 1)))
  }

//   |-------|-------|-------|-------|
//   | ##### | (0,1) | ##### | (0,3) |
//   |-------|-------|-------|-------|
//   | ##### | (1,1) | ##### | (1,3) |
//   |-------|-------|-------|-------|
//   | ##### | (2,1) | ##### | (2,3) |
//   |-------|-------|-------|-------|
  test("return an empty path when there's no path to the target") {
    val obstacles = List((0, 0), (1, 0), (2, 0), (0, 2), (1, 2), (2, 2))
    val noWayOut = Grid.unweighted(rows = 3, columns = 4, obstacles = obstacles)

    val result = shortestPath(start = (0, 1), end = (1, 3), noWayOut)

    assertEquals(result, Nil)
  }

  test("return an empty path when start or end nodes are not part of the grid") {
    assertEquals(shortestPath(start = (0, 0), end = (8, 8), aGrid), Nil)
    assertEquals(shortestPath(start = (8, 8), end = (0, 0), aGrid), Nil)
  }
