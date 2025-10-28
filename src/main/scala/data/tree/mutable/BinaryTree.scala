package data.tree.mutable

case class BinaryTree[A](root: BinaryNode[A])

object BinaryTree:

  /**
   * Builds a balanced BST from an array, where the array elements (a_0, a_1, ..., a_(n-1)) correspond to the tree in-order
   * elements (t_0, t_1, ..., t_(n-1)).
   *
   * ===Evaluation Semantics===
   *
   * Idx: 0 1 2 3 4 5 6 7 Arr: [5, 4, 9, 2, 1, 7, -1, 8]
   *
   * build(0, 8) c: = (0 + 8) / 2 = 4 -> root = 1
   *
   * build(0, 4) c = (0 + 4) / 2 = 2 -> node = 9 build(0, 2) c = (0 + 2) / 2 = 1 -> node = 4 build(0, 1) -> Node(5) build(2, 2) ->
   * empty build(3, 4) -> Node(2) Return -> Node(9, left = 4, right = 2)
   *
   * build(5, 8) c = (5 + 8) / 2 = 6 -> node = -1 build(5, 6) -> Node(7) build(7, 8) -> Node(8, null, null, -1) Return -> Node(-1,
   * left = 7, right= 8)
   *
   * Return -> Node(1, left = 9, right = -1)
   *
   * Output: 1 / \ 9 -1 / \ / \ 4 2 7 8 / 5
   *
   * ===Formula===
   * For an interval [i, j): c = (i + j) / 2
   *
   * L = build(Ni, c) R = build(c + 1, j)
   *
   * build(i, j) = Node(array[c], L, R)
   *
   * ===Complexity===
   *   - Time: Θ(n)
   *     - T(n) = 2T(n/2) + Θ(1) = Θ(n)
   *   - Space:
   *     - Recursion depth is Θ(log n) due to balanced split at midpoint
   *     - Function is not tail recursive, but is safe for any reasonable array size.
   *     - For example, for 1_000_000 elements, depth ≈ log₂(1e6) ≈ 20.
   */
  def make[A](array: Array[A]): BinaryTree[A] =
    require(array.nonEmpty, "attempt to create tree from empty array")
    val size = array.size

    def build(i: Int, j: Int): BinaryNode[A] =
      if j - i == 1 then BinaryNode(array(i))
      else
        val c = (i + j) / 2
        val l = if i < c then build(i, c) else null
        val r = if c + 1 < j then build(c + 1, j) else null
        val node = BinaryNode(array(c), l, r, null, 0)
        if l != null then node.left.parent = node
        if r != null then node.right.parent = node
        node

    BinaryTree(build(0, size).nn)
