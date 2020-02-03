package fp.ds.greedy

import scala.annotation.tailrec

object Coins {

  type Coin = Int

  // coins must be in desc order
  def change(coins: List[Coin], amount: Int): List[Coin] = {
    if (amount <= 0) return Nil

    @tailrec
    def go(coins: List[Coin], amount: Int, result: List[Coin]): List[Coin] = coins match {
      case Nil                   => result
      case x :: xs if x > amount => go(xs, amount, result)
      case x :: _                => go(coins, amount - x, x :: result)
    }

    go(coins, amount, Nil)
  }
}
