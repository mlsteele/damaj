package compile

// AST structure definition
// See ASTTools for methods having to do with ASTs.
object AST {
  import IRShared._
  // Note: Parenthesis will be purged from the AST.

  sealed trait ASTNode

  // Root node
  // srcmap has an entry for every node in the ast.
  //        note this doesn't include ID's, Either's, or Option's
  // TODO(miles): make that true
  case class ProgramAST(
    callouts: List[CalloutDecl],
    fields: List[FieldDecl],
    methods: List[MethodDecl],
    srcmap: SourceMap) extends ASTNode

  case class CalloutDecl(id: ID) extends ASTNode
  // FieldDecl are not analogous to field_decl in the gramar.
  // Each FieldDecl is one variable, the size option determines whether it is an array.
  case class FieldDecl(dtype: DType, id: ID, size: Option[IntLiteral]) extends ASTNode

  case class MethodDecl(
    id: ID,
    args: List[MethodDeclArg],
    returns: DType,
    block: Block) extends ASTNode
  case class MethodDeclArg(dtype: DType, id: ID) extends ASTNode

  case class Block(decls: List[FieldDecl], stmts: List[Statement]) extends ASTNode

  sealed trait Statement extends ASTNode
  case class Assignment(left: Location, right: Expr) extends Statement
  // MethodCalls are both Statements and Exprs.
  case class MethodCall(id: ID, args: List[Either[StrLiteral, Expr]]) extends Statement with Expr
  case class If(condition: Expr, thenb: Block, elseb: Option[Block]) extends Statement
  case class For(id: ID, start: Expr, iter: Expr, thenb: Block) extends Statement
  case class While(condition: Expr, block: Block, max: Option[IntLiteral]) extends Statement
  case class Return(expr: Option[Expr]) extends Statement
  case object Break extends Statement
  case object Continue extends Statement

  sealed trait Expr extends ASTNode
  case class Location(id: ID, index: Option[Expr]) extends Expr
  case class BinOp(left: Expr, op: String, right: Expr) extends Expr
  case class UnaryOp(op: String, right: Expr) extends Expr
  case class Ternary(condition: Expr, left: Expr, right: Expr) extends Expr
  case class Literal(inner: CommonLiteral) extends Expr
}
