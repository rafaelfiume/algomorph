package fp.ds.greedy

import scala.annotation.tailrec

object RadioCoverage {

  type Radio = String
  type State = String

  def best(stations: Map[Radio, Set[State]])(statesToCover: Set[State]): Set[Radio] = {

    @tailrec def search(stations: Map[Radio, Set[State]], statesToCover: Set[State], bestStations: Set[Radio]): Set[Radio] = {
      if (statesToCover.isEmpty) return bestStations

      val availableStations = stations.keys
      val head = availableStations.head
      val newBestStation = availableStations.tail.foldLeft(head) { (a, b) =>
        val remainingAfterA = statesToCover diff stations(a)
        val remainingAfterB = statesToCover diff stations(b)
        if (remainingAfterA.size < remainingAfterB.size) a else b
      }
      val newlyCoveredStates = stations(newBestStation)
      search(stations.removed(newBestStation), statesToCover diff newlyCoveredStates, bestStations + newBestStation)
    }

    search(stations, statesToCover, Set.empty)
  }

}

/*
 * Here's a brief description of this algorithm:
 *
 * 1. Check the states to cover
 * 2. Pick up the station that covers more state (local optimal solution)
 * 3. Add it to the list bestStations and remove it from available stations
 * 4. Are there states uncovered? If so, repeat the process
 *
 */
