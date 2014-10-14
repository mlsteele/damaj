package compile


object IR2 {
  import IRShared._
  
  // Earlier version constrain all fields to come before methods.
  // The callouts are discarded.
  case class Program(fields: List[Field], main: Method, methods: List[Method])

  case class Field(id: ID, size: Option[Long])
  case class Method(id: ID, params: List[Field], cfg: CFG)

  // Not the same as an IR block! Cannot contain any control flow statements.
  // The lecture notes from 10/9 explain the idea.
  case class Block(stmts: List[Statement])
  type Statement = IR.Statement

  sealed trait Transition
  case class Edge(to: Block) extends Transition
  case class Fork(condition: IR.Expr, ifTrue: Block, ifFalse: Block) extends Transition

  type EdgeMap = IdentityMap[IR2.Block, IR2.Transition]
}

class IR2Printer(ir2: IR2.Program) {
  import IR2._

  val print: String = printIR2(ir2)

  private def printIR2(ir2: Program): String =
    "IR2.Program(xx- %s -xx)".format(ir2.main)
}

// A CFG is a digraph where the nodes are IR2.Blocks and the edges are IR2.Transitions.
// There is a single entry point and a single exit point, marked as start and end.
// So a control flow of a -> b -> c may look like (pseudocode)
//   CFG(a, c, {a -> Edge(b), b -> Edge(c)})
// We hold on to the end so that CFGs can be easily chained/combined
class CFG(val start: IR2.Block, val end: IR2.Block, val edges: IR2.EdgeMap) {
  import IR2._

  class CFGIntegrityError(msg: String) extends RuntimeException(msg)
  validate

  // Requires that there are no blocks in both CFGs
  // WARNING: destroys both input CFGs because edges is mutated.
  def ++(cfg:CFG): CFG = {
    val newEdges = edges ++ cfg.edges
    newEdges.put(end, Edge(cfg.start))
    new CFG(start, cfg.end, newEdges)
  }

  def traverse(from: Block): Set[Block] = {
    val rest: Set[Block] = edges(from) match {
      case None => Set()
      case Some(Edge(next)) => traverse(next)
      case Some(Fork(_, left, right)) => traverse(left) ++ traverse(right)
    }
    Set(from) union rest
  }

  override def toString: String = {
    "CFG(%s)".format(traverse(start))
  }

  private def validate() = {
    edges.keys.foreach(mustContain)
    edges.values.foreach{ _ match {
      case Edge(next) => mustContain(next)
      case Fork(_, left, right) => mustContain(left); mustContain(right)
    }}
  }

  private def mustContain(block: Block): Unit = {
    // TODO maybe traversing all the time is slow.
    traverse(start).contains(block) match {
      case true =>
      case false => throw new CFGIntegrityError(s"block not reachable $block")
    }
  }
}

object CFGFactory {
  import IR2._

  def fromStatement(stmt:IR2.Statement):CFG = {
    val block = Block(List(stmt))
    new CFG(block, block, new EdgeMap())
  }

  def nopBlock: Block = IR2.Block(List())
  def dummy: CFG =
    new CFG(nopBlock, nopBlock, new EdgeMap())
}
