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
    expr match {
      case MethodCall(method, args) => MethodCall(method, args.map(_.map(renameE)));

      case CalloutCall(callout, args) => CalloutCall(callout, args.map(_.map(renameE)));

      case BinOp(left, op, right) => BinOp(
        renameE(left),
        op,
        renameE(right)
      )

      case UnaryOp(op, operand) => UnaryOp(op, renameE(operand))

      case Ternary(cond, left, right) => Ternary(
        renameE(cond),
        renameE(left),
        renameE(right)
      )

      case load@LoadField(FieldSymbol(dtype, id, size), index) => if (id == o) {
        LoadField(FieldSymbol(dtype, n, size), index)
      } else load

      case _:LoadInt  => expr
      case _:LoadBool => expr
    }
  }

  def renameVarStmt(o: ID, n: ID)(stmt: Statement) : Statement = {
    val renameE = renameVarExpr(o, n)(_)
    val renameS = renameVarStmt(o, n)(_)
    val renameB = renameVarBlock(o, n)(_)
    stmt match {
      case Assignment(Store(FieldSymbol(dtype, id, size), index), right) =>
        id == o match {
          // Rename the store's id to n.
          case true  => Assignment(Store(FieldSymbol(dtype, n, size), index), renameE(right))
          // Leave the store alone, but make sure to renameE the right.
          case false => Assignment(Store(FieldSymbol(dtype, id, size), index), renameE(right))
        }

      case MethodCall(method, args) => MethodCall(method, args.map(_.map(renameE)));

      case CalloutCall(callout, args) => CalloutCall(callout, args.map(_.map(renameE)));

      case If(preStmts, cond, thenb, elseb) => If(
        preStmts.map(renameS),
        renameE(cond),
        renameB(thenb),
        elseb.map(renameB)
      )

      case For(id, start, iter, thenb) => For(
        id,
        renameE(start),
        renameE(iter),
        renameB(thenb)
      )

      case While(preStmts, condition, block, max) => While(
        preStmts.map(renameS),
        renameE(condition),
        renameB(block),
        max
      )

      case Return(expr) => Return(expr.map(renameE))
      case b: Break => b
      case c: Continue => c
    }
  }

  def renameVarBlock(o: ID, n: ID)(block: Block) : Block = {
    return Block(block.stmts.map(renameVarStmt(o, n)), block.fields)
  }

}
