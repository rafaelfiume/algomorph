package red.book.ch08

import red.book.ch04.{None, Option}
import red.book.ch05.Stream
import red.book.ch08.Exhaustive.*
import red.book.ch08.StreamGenerator.*
import red.book.ch08.Gen.choose
import red.book.ch08.Gen.boolean

object ExhaustiveExercise extends App:

  println("Prove property when domain is boolean")
  val booleanProp = forAll(boolean) { b =>
    b || true
  }
  Prop.run(booleanProp)

  println("Prove property when domain is byte")
  val bytes: Gen[Byte] = Gen.choose(-127, 129).map(_.toByte)
  val bytesProp = forAll(bytes) { b =>
    b.isValidByte
  }
  Prop.run(bytesProp)

  println("unfold continues when state is some and terminates when is none")
  val lists = choose(-500, 500).listOfN(choose(0, 20))

  val unfoldProp1 = forAll(lists) { list =>
    val stream: Stream[Int] = Stream.unfold(list) { // producing a stream from a list
      case Nil    => None
      case h :: t => Option(h -> t)
    }
    stream.toList == list
  }
  Prop.run(unfoldProp1)
