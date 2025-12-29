package io.rafaelfiume.algomorph.graphs

import io.rafaelfiume.algomorph.graphs.Graph.{Edge, Vertex}

trait GraphsContext:
  val a = Vertex("A")
  val b = Vertex("B")
  val c = Vertex("C")
  val d = Vertex("D")
  val e = Vertex("E")
  val f = Vertex("F")
  val g = Vertex("G")
  val h = Vertex("H")
  val i = Vertex("I")
  val j = Vertex("J")
  val z = Vertex("Z")

  val g1 = Graph.make(
    Edge.directed(a, b),
    Edge.directed(a, c),
    Edge.directed(a, d),
    Edge.directed(b, f),
    Edge.directed(c, g),
    Edge.directed(d, f),
    Edge.directed(d, j),
    Edge.directed(f, j),
    Edge.directed(g, h)
  )

  // See: https://ocw.mit.edu/courses/6-006-introduction-to-algorithms-fall-2011/e59f8a55929028498953691891229a17_MIT6_006F11_lec14.pdf
  val g2WFourEdgeTypes = Graph.make(
    Edge.directed(a, b),
    Edge.directed(b, e),
    Edge.directed(e, d),
    Edge.directed(a, d), // forward
    Edge.directed(d, b), // back
    Edge.directed(c, e), // cross
    Edge.directed(c, f),
    Edge.directed(f, f) // back
  )
  val g2TreeEdges = Set(Edge.directed(a, b), Edge.directed(b, e), Edge.directed(e, d), Edge.directed(c, f))
  val g2ForwardEdges = Set(Edge.directed(a, d))
  val g2BackEdges = Set(Edge.directed(d, b), Edge.directed(f, f))

  val g2CrossEdges = Set(Edge.directed(c, e))
  // Dag: Directed Acyclic Graph
  // See: https://ocw.mit.edu/courses/6-006-introduction-to-algorithms-fall-2011/e59f8a55929028498953691891229a17_MIT6_006F11_lec14.pdf
  val multipleDags = Graph
    .make(
      Edge.directed(g, h), // g -> h (g is a prerequisite for h - g must come before h)
      Edge.directed(a, h),
      Edge.directed(a, b),
      Edge.directed(b, c),
      Edge.directed(c, f),
      Edge.directed(d, c),
      Edge.directed(d, e),
      Edge.directed(e, f)
    )
    .add(i)
