package compile

// AST structure definition
// See ASTTools for methods having to do with ASTs.
object AST {
  // Note: Parenthesis will be purged from the AST.

  // Root node
  case class ProgramAST(
    callouts: List[CalloutDecl],
    fields: List[FieldDecl],
    methods: List[MethodDecl])

  case class CalloutDecl(id: ID)
  // FieldDecl are not analogous to field_decl in the gramar.
  // Each FieldDecl is one variable, the size option determines whether it is an array.
  case class FieldDecl(dtype: DType, id: ID, size: Option[IntLiteral])

  case class MethodDecl(
    id: ID,
    args: List[MethodDeclArg],
    returns: DType,
    block: Block)
  case class MethodDeclArg(dtype: DType, id: ID)

  case class Block(decls: List[FieldDecl], stmts: List[Statement])

  sealed abstract trait Statement
  case class Assignment(left: Location, right: Expr) extends Statement
  // MethodCalls are both Statements and Exprs. Is this ok?
  case class MethodCall(id: ID, args: List[Either[StrLiteral, Expr]]) extends Statement with Expr
  case class If(condition: Expr, then: Block, elseb: Option[Block]) extends Statement
  case class While(condition: Expr, block: Block, max: Option[BigInt]) extends Statement
  case class Return(expr: Option[Expr]) extends Statement
  case class Break() extends Statement
  case class Continue() extends Statement

  sealed abstract trait Expr
  case class Location(id: ID, index: Option[Expr]) extends Expr
  case class Length(id: ID) extends Expr
  case class BinOp(left: Expr, op: String, right: Expr) extends Expr
  case class UnaryOp(op: String, right: Expr) extends Expr
  case class Ternary(condition: Expr, left: Expr, right: Expr) extends Expr

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
