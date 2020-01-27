package red.book.ch11

import red.book.ch11.Monads._
import org.scalatest.{ FlatSpec, Matchers }
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class MonadSpec extends FlatSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  "sequence" should "---- describe what sequence does here -----" in {
    val options = List(Some(1), Some(2), None, Some(4), Some(5), Some(6))

    val res = optionMonad.sequence(options)

    res shouldBe None
  }

  it  should "---- describe what sequence does here ----- (Part ll)" in {
    val options = List(Some(1), Some(2), Some(3), Some(4), Some(5), Some(6))

    val res = optionMonad.sequence(options)

    res shouldBe Some(List(1, 2, 3, 4, 5, 6))
  }

  "traverse" should "---- describe what traverse does here -----" in {
    val ints = List(1, 2, 3, 4, 5, 6)

    val res = optionMonad.traverse(ints)(i => Some(i.toString))

    res shouldBe Some(List("1", "2", "3", "4", "5", "6"))
  }

  it should "---- describe what traverse does here ----- (Part ll)" in {
    val ints = List(1, 2, 3, 4, 5, 6)

    val res = optionMonad.traverse(ints)(i => if (i % 2 == 0) None else Some(i))

    res shouldBe None
  }

  "replicateM" should "respect a Monad behaviour" in {
    val none = None

    val res = optionMonad.replicateM(4, none)

    res shouldBe None
  }

  it should "replicate an element inside a Monad n times" in {
    val option = Some(1)

    val res = optionMonad.replicateM(5, option)

    res shouldBe Some(List(1, 1, 1, 1, 1))
  }

  it should "work with lists too" in {
    val list = List(8, 9)

    val res = listMonad.replicateM(2, list)

    res shouldBe List(List(8, 8), List(8, 9), List(9, 8), List(9, 9))
  }

  "filterM" should "filter a list" in {
    val list = List(1, 2, 3, 4, 5)
    val p: Int => Option[Boolean] = i => if(i % 2 == 0) Some(true) else Some(false)

    val res = optionMonad.filterM(list)(p)

    res shouldBe Some(List(2, 4))
  }

  it should "also works with lists" in {
    val list = List(1, 2, 3, 4, 5)
    val p: Int => List[Boolean] = i => if(i % 2 == 0) List(true) else List(false)

    val res = listMonad.filterM(list)(p)

    res shouldBe List(List(2, 4))
  }

  "compose" should "help us to compose monadic functions" in {
    val fa: Int => Option[Int] = i => Some(i * 3)
    val fb: Int => Option[String] = i => Some(i.toString)

    val res = optionMonad.compose(fa, fb)(5)

    res shouldBe Some("15")
  }

  it should "respect its context (meaning that it could interrupt the computation in case the context is Option, for example)" in {
    val fa: Int => Option[Int] = _ => None
    val fb: Int => Option[String] = _ => Some("Don't care, really!")

    val res = optionMonad.compose(fa, fb)(15)

    res shouldBe None
  }

  // 11.12
  "join" should "flatten a monad" in {
    optionMonad.join(Some(Some(5))) shouldEqual Some(5)
    optionMonad.join(Some(None)) shouldEqual None
  }

  // 11.12
  it should "be implemented in terms of flatMap" in {
    val listOfLists = List(List(1, 2, 3), List(4), List(5,6))

    val res = listMonad.join(listOfLists)

    res shouldEqual List(1,2,3,4,5,6)
  }

  // 11.17
  "a monad instance for Id" should "implement the id function" in {
    val value = 2

    val res = idMonad.unit(2)

    res shouldEqual Id(value)
//    res shouldEqual value
  }

  // 11.17
  it should "implement the flatMap function" in {
    val value = 2

    val res = idMonad.flatMap(Id(value))(x => Id(x * 3))

    res shouldEqual Id(value * 3)
//    res shouldEqual value * 3
  }

  // 11.18

}
