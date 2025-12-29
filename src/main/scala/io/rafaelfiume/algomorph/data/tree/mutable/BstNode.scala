package io.rafaelfiume.algomorph.data.tree.mutable

final private case class KeyValue[K, V](key: K, value: V)

/**
 * A binary node with key-value semantics where the Binary Search Tree property must hold: keys must be stored in ascending
 * traversal order (in-order).
 */
private[mutable] class BstNode[K, V] private (private var raw: BinaryNode[KeyValue[K, V]]):
  def key: K = raw.value.key
  def value: V = raw.value.value
  def parent: BstNode[K, V] | Null = if raw.parent == null then null else new BstNode(raw.parent.nn)
  // def setValue???

  def setParent(parent: BstNode[K, V] | Null): Unit =
    raw.parent = if parent == null then null else parent.raw

  def setLeft(subtree: BstNode[K, V] | Null): Unit =
    raw.left = if subtree == null then null else subtree.raw
    if subtree != null then subtree.parent.raw = raw

  def setRight(subtree: BstNode[K, V] | Null): Unit =
    raw.right = if subtree == null then null else subtree.raw
    if subtree != null then subtree.parent.raw = raw

  override def toString(): String = s"BstTree(key=$key, value=$value)"

private[mutable] object BstNode:

  def apply[K, V](kv: KeyValue[K, V]): BstNode[K, V] = new BstNode(BinaryNode(kv))

  def find[K, V](key: K, node: BstNode[K, V]): Option[BstNode[K, V]] = ???

  def findNext[K, V](key: K, node: BstNode[K, V]): Option[BstNode[K, V]] = ???

  def findPrevious[K, V](key: K, node: BstNode[K, V]): Option[BstNode[K, V]] = ???

  def inOrder[K, V](node: BstNode[K, V]): Iterator[BstNode[K, V]] = Traversals.inOrder(node.raw).map(new BstNode(_))

  def preOrder[K, V](node: BstNode[K, V]): Iterator[BstNode[K, V]] = ???

  def postOrder[K, V](node: BstNode[K, V]): Iterator[BstNode[K, V]] = ???

  object syntax:
    extension [K, V](node: BstNode[K, V]) def inOrder: Iterator[BstNode[K, V]] = BstNode.inOrder(node)
