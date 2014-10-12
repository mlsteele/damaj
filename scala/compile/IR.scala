package compile


// IR structure definition
// See IRTools for methods having to do with IRs.
object IR {
  import SymbolTable._
  import IRShared._

  // Root node
  case class ProgramIR(symbols: SymbolTable)

  case class Block(stmts: List[Statement], fields: SymbolTable)

  sealed trait Statement

  case class Assignment(left: Store, right: Expr) extends Statement

  // MethodCalls are both Statements and Exprs. Is this ok?
  sealed trait Call extends Statement with Expr
  case class MethodCall(
    method: MethodSymbol,
    args: List[Either[StrLiteral, Expr]]
  ) extends Call

  case class CalloutCall(
    callout: CalloutSymbol,
    args: List[Either[StrLiteral, Expr]]
  ) extends Call

  case class If(
    preStmts: List[Statement],
    condition: Expr,
    thenb: Block,
    elseb: Option[Block]
  ) extends Statement

  case class For(
    preStmts: List[Statement],
    id: ID,
    start: Expr,
    iter: Expr,
    thenb: Block
  ) extends Statement

  case class While(
    preStmts: List[Statement],
    condition: Expr,
    block: Block,
    max: Option[Long]
  ) extends Statement

  case class Return(expr: Option[Expr]) extends Statement

  case object Break extends Statement

  case object Continue extends Statement

  sealed trait Expr
  case class BinOp(left: Expr, op: BinOpType, right: Expr) extends Expr
  case class UnaryOp(op: UnaryOpType, right: Expr) extends Expr
  case class Ternary(condition: Expr, left: Expr, right: Expr) extends Expr

  sealed trait Load extends Expr
  case class LoadField(from: FieldSymbol, index: Option[Expr]) extends Load
  case class LoadInt(value: Long) extends Load
  case class LoadBool(value: Boolean) extends Load

  case class Store(to: FieldSymbol, index: Option[Expr])
}
