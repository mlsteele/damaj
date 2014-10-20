package compile

/**
  * Adds in explicit assignments to zero / false of all variables.
  */
object CombineScopes {
  import IR._
  import IRShared._
  import SymbolTable._
  import FunctionalUtils._

  def renameVarExpr(o: ID, n: ID)(expr: Expr) : Expr = {
    val renameE = renameVarExpr(o, n)(_)
    val renameS = renameVarStmt(o, n)(_)
    val renameB = renameVarBlock(o, n)(_)
    expr match {
      case MethodCall(method, args) => MethodCall(method, args.map(_.map(renameE)));

      case CalloutCall(callout, args) => CalloutCall(callout, args.map(_.map(renameE)));

      case BinOp(left, op, right) => BinOp(
        renameE(left),
        op,
        renameE(right)
      )

      case UnaryOp(op, operand) => UnaryOp(op, renameE(operand))
    }
  }

  def renameVarStmt(o: ID, n: ID)(stmt: Statement) : Statement = {
    val renameE = renameVarExpr(o, n)(_)
    val renameS = renameVarStmt(o, n)(_)
    val renameB = renameVarBlock(o, n)(_)
    stmt match {
      case If(preStmts, cond, thenb, elseb) => If(
        preStmts.map(renameS),
        renameE(cond),
        renameB(thenb),
        elseb.map(renameB)
      )
    }
  }

  def renameVarBlock(o: ID, n: ID)(block: Block) : Block = {
    return Block(block.stmts.map(renameVarStmt(o, n)), block.fields)
  }

}
