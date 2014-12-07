package compile

object GraphColor {
  import collection.mutable.Stack
  import scala.util.control.Breaks._

  def debug(msg: String) = Console.err.println(msg)

  // Color a graph. No two adjacent nodes will be colored the same.
  // If coloring is impossible, some nodes will not be colored.
  // X: type for nodes
  // C: type for colors
  // nodes: list of nodes in the graph
  // interference: whether two nodes are connected
  // degree: the number of edges connected to a node
  def color[X, C](nodes: Set[X], neighbors: X => Set[X], colors: Seq[C]): Map[X, C] = {
    val ncolors: Int                = colors.size
    var activeNodes: Set[X]         = nodes
    val nodeStack: Stack[X]         = new Stack()
    var colorMap: Map[X, C] = Map()

    // degree within the activeNodes sub-graph
    def activeDegree(node: X): Int =
      (neighbors(node) intersect activeNodes).size

    debug("color starting with %s nodes and %s colors".format(nodes.size, ncolors))
    debug("initial nodes: %s".format(nodes))
    breakable { while(true) {
      // Push all nodes n where: degree(n) < ncolors
      breakable { while(true) {
        debug("considering: %s".format(activeNodes.map{ x => "%s:%s".format(x, activeDegree(x)) }))

        val easyNodes: Set[X] = activeNodes.filter{ x => activeDegree(x) < ncolors }
        if (easyNodes.isEmpty) break

        debug("pushing nodes: %s".format(easyNodes))
        easyNodes.foreach(nodeStack.push _)
        activeNodes --= easyNodes
      } }

      // Now all nodes n are: degree(n) >= ncolors
      if (activeNodes.isEmpty) break

      // Discard (spill) a node.
      val spilled: X = activeNodes.head
      activeNodes -= spilled
      debug("discarding node: %s".format(spilled))
    } }

    // Now there are no nodes but those on the stack.
    while(nodeStack.nonEmpty) {
      // Pop a node from stack, color it differently from neighbors.
      val x = nodeStack.pop()
      val illegal: Set[C] = neighbors(x).flatMap{ y => colorMap.get(y) }
      val c = (colors.toSet -- illegal).head
      colorMap += (x -> c)
    }

    return colorMap
  }
}
