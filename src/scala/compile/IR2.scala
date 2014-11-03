package compile


object IR2 {
  import IRShared._
  import SymbolTable._

  // Earlier version constrain all fields to come before methods.
  // The callouts are discarded.
  case class Program(fields: List[Field], main: Method, methods: List[Method])

  case class Field(id: ID, size: Option[Long])
  case class Method(id: ID, params: List[Field], symbols: SymbolTable, cfg: CFG,returnType:DType )

  // Not the same as an IR block! Cannot contain any control flow statements.
  // The lecture notes from 10/9 explain the idea.
  case class Block(stmts: List[Statement], fields: SymbolTable) {
    override def equals(that:Any):Boolean = that match {
      case that: Block => this eq that
      case _ => false
    }
  }

  // in IR2, Statements can not contain control flow.
  sealed trait Statement
  case class Assignment(left: Store, right: Expr) extends Statement
  // TODO call should Load
  case class Call(id: ID, args:List[Either[StrLiteral, Expr]]) extends Statement with Expr
  case class Return(value: Option[Load]) extends Statement

  sealed trait Expr
  case class BinOp(left: Load, op: BinOpType, right: Load) extends Expr
  case class UnaryOp(op: UnaryOpType, right: Load) extends Expr
  case class Ternary(condition: Load, left: Load, right: Load) extends Expr

  sealed trait Load extends Expr
  // hmm, should this have something other than a FieldSymbol?
  case class LoadField(from: FieldSymbol, index: Option[Load]) extends Load
  case class LoadLiteral(value: Long) extends Load

  case class Store(to: FieldSymbol, index: Option[Load])

  sealed trait Transition
  case class Edge(to: Block) extends Transition
  case class Fork(condition: Load, ifTrue: Block, ifFalse: Block) extends Transition

  type EdgeMap = IdentityMap[IR2.Block, IR2.Transition]
}

class IR2Printer(ir2: IR2.Program) {
  import IR2._

  val print: String = printIR2(ir2)

  private def printIR2(ir2: Program): String =
    "IR2.Program(xx- %s -xx)".format(ir2.main)
    //"IR2(... pretty sweet ir2, wish I knew how to print it ...)"
}

// A CFG is a digraph where the nodes are IR2.Blocks and the edges are IR2.Transitions.
// There is a single entry point and a single exit point, marked as start and end.
// So a control flow of a -> b -> c may look like (pseudocode)
//   CFG(a, c, {a -> Edge(b), b -> Ed)ge(c)})
// We hold on to the end so that CFGs can be easily chained/combined
class CFG(val start: IR2.Block, val end: IR2.Block, val edges: IR2.EdgeMap) {
  import IR2._

  val traversed = scala.collection.mutable.Set[Block]()
  
  class CFGIntegrityError(msg: String) extends RuntimeException(msg)
  // TODO(jessk): when should we validate?
  //validate()

  // Requires that there are no blocks in both CFGs
  // WARNING: destroys one of the input CFGs because edges is mutated.
  def ++(rhs: CFG): CFG = {
    val newEdges = edges ++ rhs.edges
    newEdges.put(end, Edge(rhs.start))
    new CFG(start, rhs.end, newEdges)
  }

  def traverse() : Set[Block] = traverse(start)
  
  def traverse(from: Block): Set[Block] = {
    assert(traversed != null, "yug")
    traversed.clear()
    _traverse(from)
  }

  def _traverse(from: Block): Set[Block] = {
    if (traversed contains from) return Set()
    traversed add from

    val rest: Set[Block] = edges(from) match {
      case None => Set()
      case Some(Edge(next)) => _traverse(next)
      case Some(Fork(_, left, right)) => _traverse(left) union _traverse(right)
    }
    Set(from) union rest
  }

  override def toString: String = {
    "CFG(%s)".format(traverse(start))
  }

  /*
  private def validate() = {
    //print("Validating CFG")
    mustReach(end)
    edges.keys.foreach(mustReach)
    edges.values.foreach{ _ match {
      case Edge(next) => mustReach(next)
      case Fork(_, left, right) => mustReach(left); mustReach(right)
    }}
    //print("Finished validating")
  }
  */

  /*
  private def mustReach(block: Block): Unit = {
    // TODO maybe traversing all the time is slow.
    traverse(start).contains(block) match {
      case true =>
      case false => throw new CFGIntegrityError(s"block not reachable $block")
    }
  }
  */
}

object CFGFactory {
  import IR2._
  import SymbolTable._

  def fromStatement(stmt: IR2.Statement, fields: SymbolTable): CFG = {
    val block = Block(List(stmt), fields)
    new CFG(block, block, new EdgeMap())
  }

  def nopBlock: Block = IR2.Block(List(), new SymbolTable())
  def dummy: CFG = {
    val b = nopBlock
    new CFG(b, b, new EdgeMap())
  }

  def chain(cfgs: TraversableOnce[CFG]): CFG =
    cfgs.fold(dummy)(_ ++ _)
}
