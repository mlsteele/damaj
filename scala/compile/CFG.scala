package compile

object CFG {
  import SymbolTable._
  import IR._
  import IRShared._

  case class Program(fields: List[FieldSymbol], methods: List[MethodSymbol])
  
  // Not the same as an IR block! Cannot contain any control flow statements.
  // The lecture notes from 10/9 explain the idea.
  case class Block(stmts: List[Statement], symbols: SymbolTable, next: Transition)
  type Statement = IR.Statement

  sealed trait Transition
  case class Edge(to: Block) extends Transition
  case class Fork(condition: IR.Expr, ifTrue: Block, ifFalse: Block) extends Transition
  case class End() extends Transition

}
