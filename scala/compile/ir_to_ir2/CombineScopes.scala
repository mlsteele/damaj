package compile

/**
  * Adds in explicit assignments to zero / false of all variables.
  */
object CombineScopes {
  import IR._
  import IRShared._
  import SymbolTable._
  import FunctionalUtils._

  /**
    * Renames all references to the variable oldName with newName in
    * the given expression.
    */
  def renameVarExpr(oldName: ID, newName: ID)(expr: Expr) : Expr = {
    val renameE = renameVarExpr(oldName, newName)(_)
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

      case load@LoadField(FieldSymbol(dtype, id, size), index) => if (id == oldName) {
        LoadField(FieldSymbol(dtype, newName, size), index)
      } else load

      case _:LoadInt  => expr
      case _:LoadBool => expr
    }
  }

  /**
    * Renames all references to the variable oldName with newName in
    * the given statement.
    */
  def renameVarStmt(oldName: ID, newName: ID)(stmt: Statement) : Statement = {
    val renameE = renameVarExpr(oldName, newName)(_)
    val renameS = renameVarStmt(oldName, newName)(_)
    val renameB = renameVarBlock(oldName, newName)(_)
    stmt match {
      case Assignment(Store(FieldSymbol(dtype, id, size), index), right) =>
        id == oldName match {
          // Rename the store's id to n.
          case true  => Assignment(Store(FieldSymbol(dtype, newName, size), index), renameE(right))
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

  /**
    * Renames all references to the variable oldName with newName in
    * the given block.
    */
  def renameVarBlock(oldName: ID, newName: ID)(block: Block) : Block = {
    return Block(block.stmts.map(renameVarStmt(oldName, newName)), block.fields)
  }

}
