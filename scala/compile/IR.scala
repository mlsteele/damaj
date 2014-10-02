package compile


// IR structure definition
// TODO See IRTools for methods having to do with IRs.
object IR {
  import SymbolTable._
  import IRShared._

  // Root node
  case class ProgramIR(symbols: SymbolTable)

  case class Block(stmts: List[Statement], fields: SymbolTable)

  sealed trait Statement

  case class Assignment(left: Store, right: Expr) extends Statement
  // MethodCalls are both Statements and Exprs. Is this ok?
  // TODO(jessk): MethodCall can't take a StrLiteral
  sealed trait Call extends Statement with Expr
  case class MethodCall(method: MethodSymbol, args: List[Either[StrLiteral, Expr]]) extends Call
  case class CalloutCall(callout: CalloutSymbol, args: List[Either[StrLiteral, Expr]]) extends Call
  case class If(condition: Expr, then: Block, elseb: Option[Block]) extends Statement
  case class For(id: ID, start: Expr, iter: Expr, then: Block) extends Statement
  //TODO(andres) decide whether or not we want an intliteral
  case class While(condition: Expr, block: Block, max: Option[IntLiteral]) extends Statement
  case class Return(expr: Option[Expr]) extends Statement
  case object Break extends Statement
  case object Continue extends Statement

  sealed trait Expr
  case class BinOp(left: Expr, op: String, right: Expr) extends Expr
  // Note: Length is now a unary op
  case class UnaryOp(op: String, right: Expr) extends Expr
  case class Ternary(condition: Expr, left: Expr, right: Expr) extends Expr

  sealed trait Load extends Expr
  case class LoadField(from: FieldSymbol, index: Option[Expr]) extends Load
  case class LoadLiteral(inner: CommonLiteral) extends Load
  case class Store(to: FieldSymbol, index: Option[Expr]) extends Expr
}
