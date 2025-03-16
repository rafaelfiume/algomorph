package fp.ds

// Learning Functional Structure and Algorithm

sealed trait BinTree[+A]
case object Leaf extends BinTree[Nothing] // a terminator node like Nil for the List data type
case class Branch[A](value: A, left: BinTree[A], right: BinTree[A]) extends BinTree[A]

object Tree:

  def apply[A](ls: List[A]): BinTree[A] = ls match
    case Nil => Leaf
    case x :: xs =>
      val n = xs.length / 2
      Branch(x, apply(xs.take(n)), apply(xs.drop(n)))

  def completeTree(value: Int, depth: Int): BinTree[Int] =
    if depth == 0 then Leaf
    else
      Branch(
        value,
        completeTree(2 * value, depth - 1),
        completeTree(2 * value + 1, depth - 1)
      )

  def size[A](tree: BinTree[A]): Int = tree match
    case Leaf            => 0
    case Branch(_, l, r) => 1 + size(l) + size(r)

  def depth[A](tree: BinTree[A]): Int = tree match
    case Leaf            => 0
    case Branch(_, l, r) => 1 + (depth(l).max(depth(r)))

  def flip[A](t: BinTree[A]): BinTree[A] = t match
    case Leaf            => t
    case Branch(v, l, r) => Branch(v, flip(r), flip(l))

  def equal[A](aTree: BinTree[A], anotherTree: BinTree[A]): Boolean = (aTree, anotherTree) match
    case (Leaf, Leaf)                                         => true
    case (Branch(v1, l1, r1), Branch(v2, l2, r2)) if v1 == v2 => equal(l1, l2) && equal(r1, r2)
    case _                                                    => false

  def flippedEqual[A](tree: BinTree[A], flipped: BinTree[A]): Boolean = (tree, flipped) match
    case (Leaf, Leaf)                                         => true
    case (Branch(v1, l1, r1), Branch(v2, l2, r2)) if v1 == v2 => flippedEqual(l1, r2) && flippedEqual(r1, l2)
    case _                                                    => false

  def preorder[A](tree: BinTree[A]): List[A] = tree match
    case Leaf            => List.empty[A]
    case Branch(v, l, r) => v :: preorder(l) ++ preorder(r)

  def inorder[A](tree: BinTree[A]): List[A] = tree match
    case Leaf            => List.empty[A]
    case Branch(v, l, r) => inorder(l) ++ (v :: inorder(r))

  def postorder[A](tree: BinTree[A]): List[A] = tree match
    case Leaf            => List.empty[A]
    case Branch(v, l, r) => postorder(l) ++ postorder(r) ++ List(v)

  def preorderAcc[A](tree: BinTree[A]): List[A] =
    def go(tree: BinTree[A], acc: List[A]): List[A] = tree match
      case Leaf            => acc
      case Branch(v, l, r) => v :: go(l, go(r, acc))
    go(tree, List.empty)

  def inorderAcc[A](tree: BinTree[A]): List[A] =
    def go(tree: BinTree[A], acc: List[A]): List[A] = tree match
      case Leaf            => acc
      case Branch(v, l, r) => go(l, v :: go(r, acc))
    go(tree, List.empty)

  def postorderAcc[A](tree: BinTree[A]): List[A] =
    def go(tree: BinTree[A], acc: List[A]): List[A] = tree match
      case Leaf => acc
      case Branch(v, l, r) =>
        println(s"Branch($v, $l, $r - $v :: $acc")
        go(l, go(r, v :: acc))
    go(tree, List.empty)
