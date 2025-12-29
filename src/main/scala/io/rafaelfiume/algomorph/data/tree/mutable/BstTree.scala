package io.rafaelfiume.algomorph.data.tree.mutable

class BstTree[K, V](val root: BstNode[K, V])

object BstTree:

  /**
   * Builds a balanced BST from an array.
   *
   * ===Complexity===
   *   - Time: Θ(n log n) - dominated by sorting.
   *   - Space:
   *     - Recursion depth is Θ(log n) due to balanced split at midpoint
   *     - Function is not tail recursive, but is safe for any reasonable array size.
   *     - For example, for 1_000_000 elements, depth ≈ log₂(1e6) ≈ 20.
   */
  def make[K: Ordering, V](values: Array[V], f: V => K): BstTree[K, V] =
    val keyValues = values.map(v => KeyValue(f(v), v)).sortBy(_.key)

    def build(i: Int, j: Int): BstNode[K, V] =
      if j - i == 1 then BstNode(keyValues(i))
      else
        val c = (i + j) / 2
        val l = if i < c then build(i, c) else null
        val r = if c + 1 < j then build(c + 1, j) else null
        val n = BstNode(keyValues(c))
        if l != null then
          l.setParent(n)
          n.setLeft(l)
        if r != null then
          r.setParent(n)
          n.setRight(r)
        n

    BstTree(build(0, values.size))
