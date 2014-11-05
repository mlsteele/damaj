// Grapher:
// Takes a CFG and prints out a representation of the graph that
// dot (graphviz) can read.
//
// This involves going through every block in the edge map and
// running a generateGraph on the GFG

package compile;
//Example Usage:
//  val graph = new GraphGen(gfg).gen
//

//miles notes: create a function graph( cfg,file_name)
//that bascially runs GraphGen on the cfg and puts it into file_name
class GraphGen(cfg: CFG){
  
  // take in two nodes, and creates a directed edge from a to b
  def edgePrint(a: String, b:String): String = " \"%s\" -> \"%s\";".format(a,b)

  type Pair = (IR2.Block, IR2.Transition) 
  val graph = generateGraph(cfg)
  def generateEdges(pair:Pair): List[String] = {
    pair match{
      case (block:IR2.Block , edge: IR2.Edge) =>
        List(edgePrint(block.toString,edge.to.toString))
      case (block:IR2.Block, fork: IR2.Fork)  =>
        List(
          edgePrint(block.toString, fork.ifTrue.toString),
          edgePrint(block.toString, fork.ifFalse.toString))
      }
  }

  // CFG has attributes: start, end, and edges (edgemap).
  def generateGraph(cfg: CFG): String= {
    val pairs: List[Pair]= cfg.edges.toList // list of tuples (keys,values) 
    file(pairs.flatMap(generateEdges).mkString("\n\n"))
  }
  def file(graph: String): String = {
    s"digraph G{ \nnode [shape=rectangle] \n $graph }"
  }
}
