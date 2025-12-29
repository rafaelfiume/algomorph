package io.rafaelfiume.algomorph.data.tree.mutable

import scala.annotation.tailrec

/**
 * A general purpose mutable binary node.
 */
private[mutable] class BinaryNode[A] private (
  var value: A,
  var left: BinaryNode[A] | Null,
  var right: BinaryNode[A] | Null,
  var parent: BinaryNode[A] | Null
):
  @inline def hasLeft: Boolean = left != null
  @inline def hasRight: Boolean = right != null
  @inline def isRoot: Boolean = parent == null
  @inline def isLeftChild: Boolean = this.parent != null && this.parent.nn.left == this
  @inline def isRightChild: Boolean = this.parent != null && this.parent.nn.right == this

  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
  override def hashCode(): Int = System.identityHashCode(this)

  override def toString(): String = s"BinaryTree($value)"

private[mutable] object BinaryNode:

  def apply[A](value: A): BinaryNode[A] = new BinaryNode[A](value, null, null, null)

  def apply[A](
    value: A,
    left: BinaryNode[A] | Null,
    right: BinaryNode[A] | Null,
    parent: BinaryNode[A] | Null
  ): BinaryNode[A] = new BinaryNode[A](value, left, right, parent)

  /**
   * ===Complexity===
   *   - Time: Θ(h)
   */
  @tailrec
  def first[A](tree: BinaryNode[A]): BinaryNode[A] =
    if !tree.hasLeft then tree else first(tree.left.nn)

  /**
   * ===Complexity===
   *   - Time: Θ(h)
   */
  @tailrec
  def last[A](tree: BinaryNode[A]): BinaryNode[A] =
    if !tree.hasRight then tree else last(tree.right.nn)

  /**
   * Predecessor of node `x` is either:
   *   - The last node in the left subtree of `x` if there is one
   *   - The first ancestor `p` of `x`, where `x` is in p's right subtree
   *
   * ===Complexity===
   *   - Time: Θ(h)
   *   - Space: Θ(1)
   */
  def predecessor[A](node: BinaryNode[A]): Option[BinaryNode[A]] =
    @tailrec
    def climb(n: BinaryNode[A]): Option[BinaryNode[A]] =
      if n.isRoot then None
      else if n.isRightChild then Some(n.parent.nn)
      else climb(n.parent.nn)

    if node.hasLeft then Some(last(node.left.nn))
    else climb(node)

  /**
   * Successor of `x` is either:
   *   - The first node in the right subtree of `x` if there is one
   *   - The first ancestor `p` of `x`, where `x` is in p's left subtree
   *
   * ===Complexity===
   *   - Time: Θ(h)
   *   - Space: Θ(1)
   */
  def successor[A](node: BinaryNode[A]): Option[BinaryNode[A]] =
    @tailrec
    def climb(n: BinaryNode[A]): Option[BinaryNode[A]] =
      if n.isRoot then None
      else if n.isLeftChild then Some(n.parent.nn)
      else climb(n.parent.nn)

    if node.hasRight then Some(first(node.right.nn))
    else climb(node)

  /**
   * Inserts node `b` before node `a`.
   *
   *   - If `a` does not have left child, simply add `b` as a left child of `a`
   *   - Otherwise, insert `b` as the right child of the last node in the left subtree of `a` (which cannot have a right child).
   *
   * ===Complexity===
   *   - Time: Θ(h)
   */
  def insertBefore[A](a: BinaryNode[A], b: BinaryNode[A]): BinaryNode[A] =
    if a.hasLeft then
      val l = last(a.left)
      b.parent = l
      l.right = b
    else
      b.parent = a
      a.left = b
    b

  /**
   * Inserts node `b` after node `a`.
   *
   *   - If `a` does not have right child, simply add `b` as a right child of `a`
   *   - Otherwise, insert `b` as the left child of the first node in the right subtree of `a` (which cannot have a left child).
   *
   * ===Complexity===
   *   - Time: Θ(h)
   */
  def insertAfter[A](a: BinaryNode[A], b: BinaryNode[A]): BinaryNode[A] =
    if a.hasRight then
      val f = first(a.right)
      b.parent = f
      f.left = b
    else
      b.parent = a
      a.right = b
    b

  /**
   * Deletes `node` from its binary search tree in-place.
   *
   * ===Complexity===
   *   - Time: Θ(h)
   *   - Space: Θ(1) - no additional space is required
   */
  @tailrec
  def delete[A](node: BinaryNode[A]): BinaryNode[A] =
    def swapValues[A](a: BinaryNode[A], b: BinaryNode[A]): Unit =
      val (aValue, bValue) = (a.value, b.value)
      a.value = bValue
      b.value = aValue

    if node.hasLeft then
      val pred = predecessor(node).get
      swapValues(pred, node)
      delete(pred)
    else if node.hasRight then
      val succ = successor(node).get
      swapValues(succ, node)
      delete(succ)
    else
      if node.isLeftChild then node.parent.left = null
      else if node.isRightChild then node.parent.right = null
      node.left = null
      node.right = null
      node.parent = null
      node
