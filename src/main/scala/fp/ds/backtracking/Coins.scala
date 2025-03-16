package fp.ds.backtracking

object Coins:

  type Coin = Int

  def changeCombs(coins: List[Coin], amount: Int): List[List[Coin]] =
    def go(coins: List[Coin], amount: Int, result: List[Coin]): List[List[Coin]] =
      if amount < 0 then return Nil
      if amount == 0 then return List(result)

      coins match
        case Nil     => Nil
        case x :: xs => go(coins, amount - x, x :: result) ++ go(xs, amount, result)

    go(coins, amount, Nil)
