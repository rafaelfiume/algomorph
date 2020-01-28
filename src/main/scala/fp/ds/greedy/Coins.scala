package fp.ds.greedy

import scala.annotation.tailrec

object Coins {

  type Coin = Int

  def change(coins: List[Coin], amount: Int): List[Coin] = {
    // coins should be in desc order
    if (amount <= 0) return List.empty

    @tailrec
    def go(coins: List[Coin], amount: Int, result: List[Coin]): List[Coin] = coins match {
      case Nil                   => result
      case x :: xs if x > amount => go(xs, amount, result)
      case x :: _                => go(coins, amount - x, x :: result)
    }

    go(coins, amount, List.empty)
  }
}
