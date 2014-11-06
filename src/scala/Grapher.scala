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
class GraphGen(cfg: CFG, annotate : Option[IR2.Block => String], title: String){

  type Pair = (IR2.Block, IR2.Transition)

  val graph: String = generateGraph(cfg)

  // take in two nodes, and creates a directed edge from a to b
  def nodeShape(name:String, shape:String) = " \"%s\" [shape=%s] ".format(name,shape)

  def nodeColor(name: String, color:String) =" \"%s\" [style=filled,fillcolor=%s] ".format(name,color)

  def edgePrint(a: String, b:String): String = " \"%s\" -> \"%s\";".format(a,b)

  def nodePrint(a: String): String = " \"%s\";".format(a)

  def conditionPrint(cond: String, t: String, f: String):String =
    " \"%s\" -> \"%s\" [label=\"T\"];\n \"%s\" -> \"%s\" [label=\"F\"];".format(cond,t,cond,f)

  def generateEdges(pair: Pair): List[String] = {
    val edges = pair match{
      case (block: IR2.Block, edge: IR2.Edge) =>
        List(edgePrint(block.toString,edge.to.toString))
      case (block: IR2.Block, fork: IR2.Fork)  =>
        List(nodeShape(fork.condition.toString,"circle"),
          conditionPrint(fork.condition.toString,fork.ifTrue.toString,fork.ifFalse.toString),
          edgePrint(block.toString, fork.condition.toString))
      }
    return edges
  }

  def annotateBlock(block: IR2.Block) : List[String] = {
    annotate match {
      case Some(func) => List(
        nodeColor(func(block), "lemonchiffon"),
        edgePrint(block.toString, func(block))
      )
      case None       => List()
    }
  }

  // CFG has attributes: start, end, and edges (edgemap).
  def generateGraph(cfg: CFG): String= {
    val pairs: List[Pair] = cfg.edges.toList // list of tuples (keys,values)
    // Special case one-node graphs to explicitly display the node.
    // TODO(miles): support annotation in single-block graphs.
    val edges = pairs.isEmpty match {
      case true =>
        nodePrint(cfg.start.toString)
      case false =>
        val entries = pairs.flatMap(generateEdges)
        val start   = nodeColor(cfg.start.toString, "lightgreen")
        val end     = nodeColor(cfg.end.toString, "plum")
        (entries :+ start :+ end).mkString("\n\n")
    }
    val annotations = cfg.blocks.toList.flatMap(annotateBlock).mkString("\n\n")
    return file(edges ++ annotations)
  }

  def file(graph: String): String = {
    val titleNode = "\"%s\" [shape=tripleoctagon]".format(title)
    s"digraph G{ \nnode [shape=rectangle] \n $titleNode \n $graph }"
  }

}

object Grapher {
  private val graphDir = "tmp"

  def graph(program: IR2.Program, prefix: String): Unit = graph(program, prefix, None)

  def graph(program: IR2.Program, prefix: String, annotate: Option[IR2.Block => String]): Unit = {
    graph(program.main, prefix, annotate)
    program.methods.foreach{ method => graph(method, prefix, annotate) }
  }

  def graph(method: IR2.Method, prefix: String): Unit = graph(method, prefix, None)

  def graph(method: IR2.Method, prefix: String, annotate: Option[IR2.Block => String]) : Unit = {
    val fileName = "%s/%s.%s.gv".format(graphDir, prefix, method.id)
    val title = "%s.%s".format(prefix, method.id)
    val graphSrc = new GraphGen(method.cfg, annotate, title).graph
    val file = new java.io.PrintStream(new java.io.FileOutputStream(fileName))
    file.print(graphSrc)
  }
}
