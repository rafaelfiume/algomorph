package red.book.ch11

import red.book.ch06.State

object MonadEx extends App:

  val stateMn: State[Int, Int] = Monads.stateMonad[Int].unit(0)

  val replicated: State[Int, List[Int]] = Monads.stateMonad[Int].replicateM(4, stateMn)

  val sequenced = Monads.stateMonad[Int]
