package compile


// IR structure definition
// TODO See IRTools for methods having to do with IRs.
object IR {
  import SymbolTable._
  import IRShared._

  // Root node
  case class ProgramIR(
    callouts: SymbolTable,
    fields: SymbolTable,
    methods: SymbolTable)

  case class Block(stmts: List[Statement], symbolTable: SymbolTable)

  sealed trait Statement
  case class Assignment(left: Store, right: Expr) extends Statement
  // MethodCalls are both Statements and Exprs. Is this ok?
  case class MethodCall(id: ID, args: List[Either[StrLiteral, Expr]]) extends Statement with Expr
  case class If(condition: Expr, then: Block, elseb: Option[Block]) extends Statement
  case class For(id: ID, start: Expr, iter: Expr, then: Block) extends Statement
  case class While(condition: Expr, block: Block, max: Option[BigInt]) extends Statement
  case class Return(expr: Option[Expr]) extends Statement
  case object Break extends Statement
  case object Continue extends Statement

  sealed trait Expr
  case class BinOp(left: Expr, op: String, right: Expr) extends Expr
  // Note: Length is now a unary op
  case class UnaryOp(op: String, right: Expr) extends Expr
  case class Ternary(condition: Expr, left: Expr, right: Expr) extends Expr
  case class Literal(inner: CommonLiteral) extends Expr

  sealed trait Load extends Expr
  case class LoadField(from: FieldSymbol, index: Option[Expr]) extends Load
  case class LoadLiteral(value: IntLiteral) extends Load
  case class Store(to: FieldSymbol, index: Option[Expr]) extends Expr
}
