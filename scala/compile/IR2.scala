package compile


object IR2 {
  import SymbolTable._
  import IR._
  import IRShared._
  
  // Earlier version constrain all fields to come before methods.
  // The callouts are discarded.
  case class Program(fields: List[Field], methods: List[Method])

  case class Field(id: ID, size: Option[Long])
  case class Method(id: ID, params: List[Field], cfg: CFG)

  // Not the same as an IR block! Cannot contain any control flow statements.
  // The lecture notes from 10/9 explain the idea.
  case class Block(stmts: List[Statement])
  type Statement = IR.Statement

  sealed trait Transition
  case class Edge(to: Block) extends Transition
  case class Fork(condition: IR.Expr, ifTrue: Block, ifFalse: Block) extends Transition

  type edgeMap = IdentityMap[IR2.Block, IR2.Transition]

}

// A CFG is a digraph where the nodes are IR2.Blocks and the edges are IR2.Transitions.
// There is a single entry point and a single exit point, marked as start and end.
// So a control flow of a -> b -> c may look like (pseudocode)
//   CFG(a, c, {a -> Edge(b), b -> Edge(c)})
// We hold on to the end so that CFGs can be easily chained/combined
class CFG(val start:IR2.Block, val end:IR2.Block, val edges:IR2.edgeMap) {

  // Requires that there are no blocks in both CFGs
  def ++(cfg:CFG): CFG = {
    val newEdges = edges ++ cfg.edges
    newEdges.put(end, IR2.Edge(cfg.start))
    new CFG(start, cfg.end, newEdges)
  }

}

object CFGFactory {
  def fromStatement(stmt:IR2.Statement):CFG = {
    val block = IR2.Block(List(stmt))
    new CFG(block, block, new IR2.edgeMap())
  }

  val dummy = new CFG(IR2.Block(List()), IR2.Block(List()), new IdentityMap[IR2.Block, IR2.Transition]())
}
