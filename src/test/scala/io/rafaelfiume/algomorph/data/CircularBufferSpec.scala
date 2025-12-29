package io.rafaelfiume.algomorph.data

import munit.FunSuite

class CircularBufferSpec extends FunSuite:

  test("evicts oldest element after full capacity in FIFO order"):
    val buf0 = CircularBuffer.make[Int](size = 3)
    val (buf1, e1) = buf0.add(1); assertEquals(e1, expected = None)
    val (buf2, e2) = buf1.add(2); assertEquals(e2, expected = None)
    val (buf3, e3) = buf2.add(3); assertEquals(e3, expected = None)
    val (buf4, e4) = buf3.add(4); assertEquals(e4, expected = Some(1))
    val (buf5, e5) = buf4.add(5); assertEquals(e5, expected = Some(2))
    val (buf6, e6) = buf5.add(6); assertEquals(e6, expected = Some(3))
    val (_, e7) = buf6.add(7); assertEquals(e7, expected = Some(4))

  test("mutable buffer evicts oldest element after full capacity in FIFO order"):
    val buffer = mutable.CircularBuffer.make[Int](3)
    assertEquals(buffer.add(1), expected = None)
    assertEquals(buffer.add(2), expected = None)
    assertEquals(buffer.add(3), expected = None)
    assertEquals(buffer.add(4), expected = Some(1))
    assertEquals(buffer.add(5), expected = Some(2))
    assertEquals(buffer.add(6), expected = Some(3))
    assertEquals(buffer.add(7), expected = Some(4))

  test("iterates over the buffer's elements in least-recent insertion order"):
    val buffer = mutable.CircularBuffer.make[Int](3)
    assert(buffer.iterator.isEmpty)
    val _ = buffer.add(2)
    assertEquals(buffer.iterator.toList, expected = scala.collection.immutable.List(2))
    val _ = buffer.add(4)
    assertEquals(buffer.iterator.toList, expected = scala.collection.immutable.List(2, 4))
    val _ = buffer.add(6)
    val _ = buffer.add(8)
    val _ = buffer.add(10)
    assertEquals(buffer.iterator.toList, expected = scala.collection.immutable.List(6, 8, 10))
