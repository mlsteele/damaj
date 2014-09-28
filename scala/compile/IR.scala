package compile

import SymbolTable._

// IR structure definition
// TODO See IRTools for methods having to do with IRs.
object IR {

  // Root node
  case class ProgramIR(
    symbols: SymbolTable
  )

  case class Block(stmts: List[Statement], symbolTable: SymbolTable)

  sealed abstract trait Statement
  case class Assignment(left: Store, right: Expr) extends Statement
  // MethodCalls are both Statements and Exprs. Is this ok?
  case class MethodCall(id: ID, args: List[Either[StrLiteral, Expr]]) extends Statement with Expr
  case class If(condition: Expr, then: Block, elseb: Option[Block]) extends Statement
  case class While(condition: Expr, block: Block, max: Option[BigInt]) extends Statement
  case class Return(expr: Option[Expr]) extends Statement
  case class Break() extends Statement
  case class Continue() extends Statement

  sealed abstract trait Expr
  case class BinOp(left: Expr, op: String, right: Expr) extends Expr
  // Note: Length is now a unary op
  case class UnaryOp(op: String, right: Expr) extends Expr
  case class Ternary(condition: Expr, left: Expr, right: Expr) extends Expr

  sealed abstract trait Load extends Expr
  case class LoadField(from: Field, index: Option[Expr]) extends Load
  case class LoadLiteral(value: IntLiteral) extends Load
  case class Store(to: Field, index: Option[Expr]) extends Expr

  type ID = String
  sealed trait DType
  case object DTVoid extends DType
  case object DTInt extends DType
  case object DTBool extends DType

  sealed abstract trait Literal extends Expr
  case class IntLiteral(value: BigInt) extends Literal
  case class CharLiteral(value: Char) extends Literal
  case class BoolLiteral(value: Boolean) extends Literal
  // String literal is not a literal! It's only used in callout args.
  case class StrLiteral(value: String)
}
