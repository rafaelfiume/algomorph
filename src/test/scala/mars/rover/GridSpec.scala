package mars.rover

import mars.rover.Grid.{Edge, printPath}
import munit.Assertions.*
import munit.FunSuite

class GridSpec extends FunSuite:

  /*
Rover will always try to turn 'left' first, then 'up', 'down' and, finally, 'right'.

            |-------|-------|-------|-------|-------|
            | (0,0) | (0,1) | (0,2) | (0,3) | (0,4) |
            |-------|-------|-------|-------|-------|
            | (1,0) | (1,1) | (1,2) | (1,3) | (1,4) |
            |-------|-------|-------|-------|-------|
            | (2,0) | (2,1) | (2,2) | (2,3) | (2,4) |
            |-------|-------|-------|-------|-------|
   */
  test("Grid apply creates a grid (adjacency list) with R x C size") {
    val aGrid = Grid.unweighted(rows = 3, columns = 5)

    assertEquals(
      aGrid,
      Vector(
        (0, 0) -> List((0, 4), (2, 0), (1, 0), (0, 1)),
        (0, 1) -> List((0, 0), (2, 1), (1, 1), (0, 2)),
        (0, 2) -> List((0, 1), (2, 2), (1, 2), (0, 3)),
        (0, 3) -> List((0, 2), (2, 3), (1, 3), (0, 4)),
        (0, 4) -> List((0, 3), (2, 4), (1, 4), (0, 0)),
        (1, 0) -> List((1, 4), (0, 0), (2, 0), (1, 1)),
        (1, 1) -> List((1, 0), (0, 1), (2, 1), (1, 2)),
        (1, 2) -> List((1, 1), (0, 2), (2, 2), (1, 3)),
        (1, 3) -> List((1, 2), (0, 3), (2, 3), (1, 4)),
        (1, 4) -> List((1, 3), (0, 4), (2, 4), (1, 0)),
        (2, 0) -> List((2, 4), (1, 0), (0, 0), (2, 1)),
        (2, 1) -> List((2, 0), (1, 1), (0, 1), (2, 2)),
        (2, 2) -> List((2, 1), (1, 2), (0, 2), (2, 3)),
        (2, 3) -> List((2, 2), (1, 3), (0, 3), (2, 4)),
        (2, 4) -> List((2, 3), (1, 4), (0, 4), (2, 0))
      )
    )
  }

  /*
A Grid may have obstacles:

            |-------|-------|-------|
            | (0,0) | (0,1) | ##### |
            |-------|-------|-------|
            | (1,0) | (1,1) | ##### |
            |-------|-------|-------|
            | ##### | (2,1) | (2,2) |
            |-------|-------|-------|
   */
  test("no connections where there are obstacles (i.e. rover shouldn't be able pass through obstacles)") {
    val aGridWithObstacles = Grid.unweighted(rows = 3, columns = 3, obstacles = List((2, 0), (0, 2), (1, 2)))

    assertEquals(
      aGridWithObstacles,
      Vector(
        (0, 0) -> List((1, 0), (0, 1)),
        (0, 1) -> List((0, 0), (2, 1), (1, 1)),
        (1, 0) -> List((0, 0), (1, 1)),
        (1, 1) -> List((1, 0), (0, 1), (2, 1)),
        (2, 1) -> List((1, 1), (0, 1), (2, 2)),
        (2, 2) -> List((2, 1))
      )
    )
  }

  /*
A Grid may be weighted (default weight is 1):

            |-------|-------|
            | (0,0) | **2** |
            |-------|-------|
            | (1,0) | **3** |
            |-------|-------|
            | (2,0) | (2,1) |
            |-------|-------|
   */
  test("weights where specified") {
    val aGrid = Grid.weighted(
      rows = 3,
      columns = 2,
      weights = List(Edge((0, 1), w = 2), Edge((1, 1), w = 3))
    )

    assertEquals(
      aGrid,
      Vector(
        (0, 0) -> List(Edge((0, 1), w = 2), Edge((2, 0), w = 1), Edge((1, 0), w = 1), Edge((0, 1), w = 2)),
        (0, 1) -> List(Edge((0, 0), w = 1), Edge((2, 1), w = 1), Edge((1, 1), w = 3), Edge((0, 0), w = 1)),
        (1, 0) -> List(Edge((1, 1), w = 3), Edge((0, 0), w = 1), Edge((2, 0), w = 1), Edge((1, 1), w = 3)),
        (1, 1) -> List(Edge((1, 0), w = 1), Edge((0, 1), w = 2), Edge((2, 1), w = 1), Edge((1, 0), w = 1)),
        (2, 0) -> List(Edge((2, 1), w = 1), Edge((1, 0), w = 1), Edge((0, 0), w = 1), Edge((2, 1), w = 1)),
        (2, 1) -> List(Edge((2, 0), w = 1), Edge((1, 1), w = 3), Edge((0, 1), w = 2), Edge((2, 0), w = 1))
      )
    )
  }

  test("printPath prints the path from one node to another") {
    assertEquals(printPath(List((0, 0), (0, 4), (2, 4))), "(0,0) -> (0,4) -> (2,4)")
    assertEquals(printPath(List.empty), "")
  }
