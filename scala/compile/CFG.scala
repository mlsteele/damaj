package compile

object CFG {
  import SymbolTable._
  import IR._
  import IRShared._
  
  // Earlier version constrain all fields to come before methods.
  // The callouts are discarded.
  case class Program(fields: List[Field], methods: List[Method])

  case class Field(id: ID, size: Option[Long])
  case class Method(id: ID, params: List[Field], start: Block, edges: IdentityMap[Block, Transition])
  
  // Not the same as an IR block! Cannot contain any control flow statements.
  // The lecture notes from 10/9 explain the idea.
  case class Block(stmts: List[Statement])
  type Statement = IR.Statement

  sealed trait Transition
  case class Edge(to: Block) extends Transition
  case class Fork(condition: IR.Expr, ifTrue: Block, ifFalse: Block) extends Transition
  case class End(from: Block) extends Transition

}
