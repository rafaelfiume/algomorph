package data.tree

import scala.annotation.tailrec

enum AvlTree[+A]:
  case Empty
  case Node[+A](value: A, left: AvlTree[A], right: AvlTree[A], height: Int) extends AvlTree[A]

object AvlTree:

  /**
   * ===Complexity===
   *   - Time: Θ(h)
   */
  @tailrec
  def first[A](tree: AvlTree[A]): Option[AvlTree[A]] = tree match
    case Empty                               => None
    case node: Node[A] if node.left == Empty => Some(node)
    case node: Node[A]                       => first[A](node.left)

  /**
   * ===Complexity===
   *   - Time: Θ(h)
   */
  @tailrec
  def last[A](tree: AvlTree[A]): Option[AvlTree[A]] = tree match
    case Empty                                => None
    case node: Node[A] if node.right == Empty => Some(node)
    case node: Node[A]                        => last[A](node.right)
