package red.book.ch08

import red.book.ch04.Some
import red.book.ch05.Stream
import red.book.ch06.RNG
import red.book.ch08.Prop.*
import red.book.ch08.StreamGenerator.StreamGenerator

import scala.language.postfixOps
import scala.math.abs

object Exhaustive:

  def forAll[A](as: Gen[A])(f: A => Boolean)(implicit stream: StreamGenerator[A], testInspector: TestInspector[A]): Prop = Prop {
    (n, rng) =>
      testInspector.generateTestCases(stream(n, rng)(as))(f)
  }

object StreamGenerator:

  type StreamGenerator[A] = (Int, RNG) => Gen[A] => Stream[(A, Int)]

  implicit def aStream[A]: StreamGenerator[A] = (n, rng) =>
    gen =>
      def randomStream(g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
      randomStream(gen)(rng).zip(Stream.from(0)).take(n)
  implicit def byteStream: StreamGenerator[Byte] = (_, _) =>
    _ => Stream.from(Byte.MinValue).map(_.toByte).zip(Stream.from(0)).take(abs(Byte.MinValue) + Byte.MaxValue + 1)
  implicit def booleanStream: StreamGenerator[Boolean] = (_, _) => _ => Stream(true, false).zip(Stream.from(0)).take(2)

object TestInspector:

  given ProvableTestInspector[Byte] = new ProvableTestInspector[Byte]
  given ProvableTestInspector[Boolean] = new ProvableTestInspector[Boolean]
  given [A]: UnfalsifiedTestInspector[List[A]] = new UnfalsifiedTestInspector[List[A]]

class ProvableTestInspector[A] extends TestInspector[A]:
  def inspectResult(s: Stream[Prop.Result]): Prop.Result = s.find(_.isFalsified).getOrElse(Proved)

class UnfalsifiedTestInspector[A] extends TestInspector[A]:
  def inspectResult(s: Stream[Prop.Result]): Prop.Result = s.find(_.isFalsified).getOrElse(Passed)

trait TestInspector[A]:

  def generateTestCases(s: Stream[(A, Int)])(f: A => Boolean): Prop.Result =
    s.map { case (a, i) =>
      try if f(a) then Passed else Falsified(a.toString, i)
      catch case e: Exception => Falsified(buildMsg(a, e), i)
    } toResult

  def inspectResult(s: Stream[Prop.Result]): Prop.Result

  private def buildMsg(s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n${e.getStackTrace.mkString("\n")}"

  implicit class StreamOps(s: Stream[Prop.Result]):
    def toResult = inspectResult(s)
