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

  /**
   * Iterates over the buffered elements in least-recent insertion order.
   *
   * ===Evaluation Semantics===
   *   - Let capacity = 3 and inserted elements be: [2, 4, 6, 8, 10]
   *   - The internal buffer will contain: [8, 10, 6]
   *   - Then, `iterator` will yield: 6, 8, 10.
   *
   * ===Complexity===
   *   - Time: Θ(n) - where n = number of elements in the buffer (<= capacity)
   *   - Space: Θ(1)
   *
   * Performance Notes:
   *   - Space is Θ(1) because the returned iterator is lazy. It holds:
   *     - The function to compute the wrapped indices
   *     - The buffer's reference along with a function to return its content by index.
   *   - I.e. no collection is materialised.
   */
  def iterator: Iterator[T] =
    val n = if counter < capacity then counter else capacity
    (0 until n).iterator.map(i => (counter + i) % n).map(buffer)
