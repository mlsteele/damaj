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
 
  case class DirectedGraph[N, E] (val edges: List[DirectedEdge[N, E]]) {
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
      * Adds an edge to the graph
      */
    def +(edge: DirectedEdge[N, E]): DirectedGraph[N, E] = DirectedGraph(edges :+ edge)

    /**
      * Adds a list of edges to the graph
      */
    def ++(newEdges: TraversableOnce[DirectedEdge[N, E]]): DirectedGraph[N, E] = DirectedGraph(edges ++ newEdges)

    /**
      * Merges two graphs together
      */
    def ++(that: DirectedGraph[N, E]): DirectedGraph[N, E] = DirectedGraph(this.edges ++ that.edges)
  }
}
