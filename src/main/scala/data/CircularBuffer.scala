package data

import scala.reflect.ClassTag
import scala.collection.immutable.Queue

object CircularBuffer:
  def make[T](size: Int): CircularBuffer[T] =
    new CircularBuffer[T](0, Queue.empty, size)

object mutable:
  object CircularBuffer:
    def make[T: ClassTag](size: Int): MutableCircularBuffer[T] =
      new MutableCircularBuffer[T](size)

case class CircularBuffer[T] private (
  private val counter: Int,
  private val buffer: Queue[T],
  capacity: Int
):
  def add(value: T): (CircularBuffer[T], Option[T]) =
    if buffer.size < capacity then this.copy(counter = counter + 1, buffer.enqueue(value)) -> None
    else
      val (evicted, newQueue) = buffer.dequeue
      this.copy(counter = counter + 1, newQueue.enqueue(value)) -> Some(evicted)

  def filled: Int = if counter >= capacity then capacity else counter

/**
 * A simple and fast non-thread safe CircularBuffer. Use it when concurrence is not a requirement or for reference purposes.
 */
class MutableCircularBuffer[T: ClassTag] private[data] (capacity: Int):
  private val buffer = Array.ofDim[T](capacity)
  private var counter = 0

  def add(value: T): Option[T] =
    val i = counter % capacity
    val old = buffer(i)
    buffer(i) = value
    counter += 1
    Option.when(counter > capacity)(old)

  def filled: Int =
    if counter >= capacity then capacity else counter
