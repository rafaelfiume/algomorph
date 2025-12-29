package io.rafaelfiume.algomorph.data.tree.mutable

import io.rafaelfiume.algomorph.data.tree.mutable.BinaryNode

import scala.collection.mutable

object Traversals:

  /**
   * Traverses the binary node in ascending (in-order) order.
   *
   * ===Complexity===
   *   - Time: Θ(n) - when n = the number of nodes
   *   - Space: Θ(h) - where h = the height of the node
   */
  def inOrder[A](node: BinaryNode[A]): Iterator[BinaryNode[A]] =
    new Iterator[BinaryNode[A]]:
      private var current: BinaryNode[A] | Null = node
      private val stack: mutable.Stack[BinaryNode[A]] = mutable.Stack.empty

      override def hasNext: Boolean = current != null || stack.nonEmpty

      override def next(): BinaryNode[A] =
        if !hasNext then throw new NoSuchElementException("next on an empty iterator")

        while this.current != null do
          stack.push(this.current.nn)
          this.current = this.current.nn.left

        val node = this.stack.pop()
        this.current = node.right
        node

  /**
   * Traverses the binary node in pre-order:
   *
   *   - node -> Left(node) -> Right(node)
   *
   * Traversing the node in pre-order yields nodes in topological order, since every parent is visited (thus, processed) before
   * its left and right subtrees.
   *
   * ===Complexity===
   *   - Time: Θ(n) - when n = the number of nodes
   *   - Space: Θ(h) - where h = the height of the node
   */
  def preOrder[A](node: BinaryNode[A]): Iterator[BinaryNode[A]] =
    new Iterator[BinaryNode[A]]:
      private val stack: mutable.Stack[BinaryNode[A]] = mutable.Stack(node)

      override def hasNext: Boolean = stack.nonEmpty
      override def next(): BinaryNode[A] =
        if !hasNext then throw new NoSuchElementException("next on an empty iterator")

        val node = stack.pop
        if node.nn.right != null then stack.push(node.right.nn)
        if node.nn.left != null then stack.push(node.left.nn)
        node

  /**
   * Traverses the binary node in post-order:
   *
   *   - Left(node) -> Right(node) -> node
   *
   * Useful when child nodes need to be processed before their parents, such as postfix expression evaluation.
   *
   * ===Complexity===
   *   - Time: Θ(n) - when n = the number of nodes
   *   - Space: Θ(h) - where h = the height of the node
   */
  def postOrder[A](node: BinaryNode[A]): Iterator[BinaryNode[A]] =
    new Iterator[BinaryNode[A]]:
      private val stack: mutable.Stack[BinaryNode[A]] = mutable.Stack.empty
      private var lastVisited: BinaryNode[A] | Null = null // only set when both left and right subtree have been visited
      private var current: BinaryNode[A] | Null = node

      override def hasNext: Boolean = current != null || stack.nonEmpty

      override def next(): BinaryNode[A] =
        if !hasNext then throw new NoSuchElementException("next on an empty iterator")

        var nextNode: BinaryNode[A] | Null = null

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
