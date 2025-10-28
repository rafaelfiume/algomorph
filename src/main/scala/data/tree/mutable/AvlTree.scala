package data.tree.mutable

import scala.annotation.tailrec
import scala.collection.mutable

final case class AvlTree[A](
  val value: A,
  var left: AvlTree[A] | Null,
  var right: AvlTree[A] | Null,
  var parent: AvlTree[A] | Null,
  var height: Int
):
  def hasLeft: Boolean = left != null
  def hasRight: Boolean = right != null
  def isRoot: Boolean = parent == null
  def isLeftChild: Boolean = this.parent.nn.left == this
  def isRightChild: Boolean = this.parent.nn.right == this

object AvlTree:

  /**
   * ===Complexity===
   *   - Time: Θ(n)
   */
  def traverseInOrder[A](tree: AvlTree[A]): Iterator[AvlTree[A]] =
    new Iterator[AvlTree[A]]:
      private var current: AvlTree[A] | Null = tree
      private val stack: mutable.Stack[AvlTree[A]] = mutable.Stack.empty

      override def hasNext: Boolean = current != null || stack.nonEmpty

      // next(node.left) ++ n ++ next(right)
      override def next(): AvlTree[A] =
        while this.current != null do
          stack.push(this.current)
          this.current = this.current.left

        val node = this.stack.pop()
        this.current = node.right
        node

  /**
   * ===Complexity===
   *   - Time: Θ(h)
   */
  @tailrec
  def first[A](tree: AvlTree[A]): AvlTree[A] =
    if !tree.hasLeft then tree else first(tree.left.nn)

  /**
   * ===Complexity===
   *   - Time: Θ(h)
   */
  @tailrec
  def last[A](tree: AvlTree[A]): AvlTree[A] =
    if !tree.hasRight then tree else last(tree.right.nn)

  /**
   * Predecessor of `x` is either:
   *   - The first node in the left subtree of `x` if there is one
   *   - The first ancestor `p` of `x`, where `x` is in p's right subtree
   *
   * ===Complexity===
   *   - Time: Θ(h)
   */
  def predecessor[A](node: AvlTree[A]): Option[AvlTree[A]] =
    @tailrec
    def climb(n: AvlTree[A]): Option[AvlTree[A]] =
      println(s"climbing ${n.value}")
      if n.isRoot then None
      else if n.isRightChild then Some(n.parent.nn)
      else climb(n.parent.nn)

    if node.hasLeft then Some(first(node.left.nn))
    else climb(node)

  /**
   * Successor of `x` is either:
   *   - The first node in the right subtree of `x` if there is one
   *   - The first ancestor `p` of `x`, where `x` is in p's left subtree
   *
   * ===Complexity===
   *   - Time: Θ(h)
   */
  def successor[A](node: AvlTree[A]): Option[AvlTree[A]] =
    @tailrec
    def climb(n: AvlTree[A]): Option[AvlTree[A]] =
      if n.isRoot then None
      else if n.isLeftChild then Some(n.parent.nn)
      else climb(n.parent.nn)

    if node.hasRight then Some(first(node.right.nn))
    else climb(node)

  /**
   * Insert node `b` before node `a`.
   *
   *   - If `a` does not have left child, simply add `b` as a left child of `a`
   *   - Otherwise, insert `b` as the right child of the last node in the left subtree of `a` (which cannot have a right child).
   *
   * ===Complexity===
   *   - Time: Θ(h)
   */
  def insertBefore[A](a: AvlTree[A], b: AvlTree[A]): AvlTree[A] =
    if a.hasLeft then
      val l = last(a.left)
      b.parent = l
      l.right = b
    else
      b.parent = a
      a.left = b
    a

  /**
   * Insert node `b` after node `a`.
   *
   *   - If `a` does not have right child, simply add `b` as a right child of `a`
   *   - Otherwise, insert `b` as the left child of the first node in the right subtree of `a` (which cannot have a left child).
   *
   * ===Complexity===
   *   - Time: Θ(h)
   */
  def insertAfter[A](a: AvlTree[A], b: AvlTree[A]): AvlTree[A] =
    if a.hasRight then
      val f = first(a.right)
      b.parent = f
      f.left = b
    else
      b.parent = a
      a.right = b
    a
