package red.book.ch08

import red.book.ch04.{Option, *}
import red.book.ch08.Dsl.*
import red.book.ch08.Gen.{boolean, choose, option}
import red.book.ch08.Prop.forAll

import scala.language.postfixOps
import scala.{Either as _, None as _, Option as _, Some as _}

object SequenceProperties extends App:

  /**
   * ** sequences ***
   */

  val manyListOfOptions: Gen[List[Option[Boolean]]] = option(boolean).listOfN(choose(0, 10))

  println("sequence should convert List[Option[A]] to Option[List[A]]")
  val sequenceProp1 = forAll(manyListOfOptions) { manyListOfOptions =>

    val sequenced = Option.sequence(manyListOfOptions)
//    println(manyListOfOptions + " >>> " + sequenced)

    where(sequenced nonEmpty) {
      println("sequenced is something")
      // contain the same number of elements
      (sequenced.map(_.size) get) == manyListOfOptions.size

      // all the elements of manyListOfOptions are in sequenced
      manyListOfOptions.forall(e => sequenced.get contains e.get)

      // all the elements of sequenced are in manyListOfOption
      sequenced.get.forall(e => manyListOfOptions contains Some(e))
    }
  }

  val listContainsNoneProp = forAll(manyListOfOptions) { manyListOfOptions =>

    val sequenced = Option.sequence(manyListOfOptions)

    where(manyListOfOptions contains None) {
      println("sequenced is none!")
      sequenced == None
    }
  }

  Prop.run(sequenceProp1 && listContainsNoneProp)
