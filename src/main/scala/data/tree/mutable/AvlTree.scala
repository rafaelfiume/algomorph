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

  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
  override def hashCode(): Int = System.identityHashCode(this)

  override def toString(): String = s"AvlTree($value)"

object AvlTree:

  /**
   * Traverses the binary tree in ascending (in-order) order.
   *
   * Binary search tree (BST) invariant, where every left child <= its parent and every right child >= its parent, guarantees that
   * recursively visiting Left(node) -> node -> Right(node) yields nodes in ascending order.
   *
   * ===Complexity===
   *   - Time: Θ(n) - when `n` = number of items in the tree
   *   - Space: Θ(h) - where `h` is the height of the tree
   */
  def traverseInOrder[A](tree: AvlTree[A]): Iterator[AvlTree[A]] =
    new Iterator[AvlTree[A]]:
      private var current: AvlTree[A] | Null = tree
      private val stack: mutable.Stack[AvlTree[A]] = mutable.Stack.empty

      override def hasNext: Boolean = current != null || stack.nonEmpty

      override def next(): AvlTree[A] =
        if !hasNext then throw new NoSuchElementException("next on an empty iterator")

        while this.current != null do
          stack.push(this.current.nn)
          this.current = this.current.nn.left

        val node = this.stack.pop()
        this.current = node.right
        node

  /**
   * Traverses the binary tree in pre-order:
   *
   *   - node -> Left(node) -> Right(node)
   *
   * Traversing the tree in pre-order yields nodes in topological order, since every parent is visited (thus, processed) before
   * its left and right subtrees.
   *
   * ===Complexity===
   *   - Time: Θ(n) - when `n` = number of items in the tree
   *   - Space: Θ(h) - where `h` is the height of the tree
   */
  def traversePreOrder[A](tree: AvlTree[A]): Iterator[AvlTree[A]] =
    new Iterator[AvlTree[A]]:
      private val stack: mutable.Stack[AvlTree[A]] = mutable.Stack(tree)

      override def hasNext: Boolean = stack.nonEmpty
      override def next(): AvlTree[A] =
        if !hasNext then throw new NoSuchElementException("next on an empty iterator")

        val node = stack.pop
        if node.nn.right != null then stack.push(node.right.nn)
        if node.nn.left != null then stack.push(node.left.nn)
        node

  /**
   * Traverses the binary tree in post-order:
   *
   *   - Left(node) -> Right(node) -> node
   *
   * Useful when child nodes need to be processed before their parents, such as postfix expression evaluation.
   *
   * ===Complexity===
   *   - Time: Θ(n) - when `n` = number of items in the tree
   *   - Space: Θ(h) - where `h` is the height of the tree
   */
  def traversePostOrder[A](tree: AvlTree[A]): Iterator[AvlTree[A]] =
    new Iterator[AvlTree[A]]:
      private val stack: mutable.Stack[AvlTree[A]] = mutable.Stack.empty
      private var lastVisited: AvlTree[A] | Null = null // only set when both left and right subtree have been visited
      private var current: AvlTree[A] | Null = tree

      override def hasNext: Boolean = current != null || stack.nonEmpty

      override def next(): AvlTree[A] =
        if !hasNext then throw new NoSuchElementException("next on an empty iterator")

        var nextNode: AvlTree[A] | Null = null

        while nextNode == null do
          if current != null then
            stack.push(current.nn)
            current = current.left
          else
            val peek = stack.top
            if peek.nn.hasRight && peek.right != lastVisited then current = peek.right
            else
              nextNode = stack.pop()
              lastVisited = nextNode

        nextNode.nn

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
   * Predecessor of node `x` is either:
   *   - The first node in the left subtree of `x` if there is one
   *   - The first ancestor `p` of `x`, where `x` is in p's right subtree
   *
   * ===Complexity===
   *   - Time: Θ(h)
   */
  def predecessor[A](node: AvlTree[A]): Option[AvlTree[A]] =
    @tailrec
    def climb(n: AvlTree[A]): Option[AvlTree[A]] =
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
