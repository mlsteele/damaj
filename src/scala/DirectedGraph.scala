package compile

object DirectedGraph {
  /**
    * A directed edge with data attached to the edge.
    */
  case class DirectedEdge[N, E](from: N, to: N, value: E) {
    override def toString(): String = s"$from --> $to labeled $value"
  }

  /**
    * Import this object to get convenience methods for creating
    * directed graphs with edge data.
    * Example usage: 1 --> 2 labeled "this is an edge"
    */
  object Labeled {
    case class PartialEdge[N](from: N, to: N) {
      def labeled[E](value: E) : DirectedEdge[N, E] = DirectedEdge(from, to, value)
    }

    implicit class PartialEdgeBuilder[N](from: N) {
      def -->(to: N) : PartialEdge[N] = PartialEdge(from, to)
    }
  }

  /**
    * Import this object to get convenience methods for creating
    * directed graphs without edge data.
    * Example usage: 1 --> 2
    */
  object Unlabeled {
    implicit class EdgeBuilder[N](from: N) {
      def -->(to: N) : DirectedEdge[N, Unit] = DirectedEdge(from, to, ())
    }
  }

  /**
    * Convenience conversion for making a graph with only one edge.
    */
  implicit def edgeToGraph[N, E](edge: DirectedEdge[N, E]) : Set[DirectedEdge[N, E]] = Set(edge)

  /**
    * Because DirectedGraph is actually just an implicit conversion
    * from Set, you can use all of the normal Set operations, like
    * +: ++, map, filter...
    */
  implicit class DirectedGraph[N, E](edges: Set[DirectedEdge[N, E]]){
    type Edge = DirectedEdge[N, E]
    type EdgeMap = Map[N, Set[Edge]]

    /**
      * nodes: All the nodes in the graph
      * inEdges: Maps a node to all the edges going into the node
      * outEdges: Maps a node to all the edges going out of the node
      * 
      * Example usage to get all the outgoing edges of a node:
      * graph.outEdges(n)
      * 
      * ^ the above works because Map implements
      * apply(), so you can use maps as if they were functions.
      */
    lazy val (nodes, inEdges, outEdges): (Set[N], EdgeMap, EdgeMap) = {
      var inEdgeMap: EdgeMap = Map().withDefaultValue(Set())
      var outEdgeMap: EdgeMap = Map().withDefaultValue(Set())
      var nodeSet : Set[N] = Set()
      edges.foreach { e =>
        // Update outgoing edges
        outEdgeMap += e.from -> (outEdgeMap(e.from) + e)
        inEdgeMap  += e.to   -> (inEdgeMap (e.to  ) + e)
        nodeSet += e.from
        nodeSet += e.to
      }
      (nodeSet, inEdgeMap, outEdgeMap)
    }

    /**
      * Applies a function to all nodes in the graph.  TODO: This
      * function is implemented in a way such that functions can be
      * repeatedly called on the same node. This is only a problem if
      * the function is impure, or we care about performance.
      */
    def mapNodes[N2](f: N => N2) : Set[DirectedEdge[N2, E]] = edges.map {
      case DirectedEdge(a, b, value) => DirectedEdge(f(a), f(b), value)
    }

    /**
      * Applies a function to all of the edges values in the graph.
      */
    def mapEdgeValues[E2](f: E => E2): Set[DirectedEdge[N, E2]] = edges.map {
      case DirectedEdge(a, b, value) => DirectedEdge(a, b, f(value))
    }
  }

  /**
    * Typed synonym for an empty graph.
    */
  def emptyGraph[N, E]() : Set[DirectedEdge[N, E]] = Set()
}
