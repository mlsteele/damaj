package compile

/*
 * Peephole algebraic optimizaions.
 * Peephole means that the analysis+transformation is very local.
 * Included optimizations:
 */
object PeepholeAlgebra extends Transformation {
  import FunctionalUtils.EnhancedEither
  import IRShared._
  import IR2._

  override def transform(cfg: CFG): CFG = cfg.mapBlocks(transform)

  def transform(block: Block): Block =
    block.copy(stmts = block.stmts.map(transform))

  def transform(stmt: Statement): Statement = stmt match {
    case x:Assignment      => x.copy(right = simplify(x.right))
    case _ => stmt
  }

  private def simplify(expr: Expr): Expr = expr match {
    // 3 * 5 = 15 // constant calculation
    case BinOp(LoadLiteral(x), op:BinOpType, LoadLiteral(y)) =>
      LoadLiteral(evalBinOp(x, op, y))
    // x * 0 = 0
    case BinOp(LoadLiteral(0), Multiply, _             ) => LoadLiteral(0)
    case BinOp(_,              Multiply, LoadLiteral(0)) => LoadLiteral(0)
    // x * 1 = x
    case BinOp(LoadLiteral(1), Multiply, right         ) => right
    case BinOp(left,           Multiply, LoadLiteral(1)) => left
    // x / 1 = x
    case BinOp(left, Divide, LoadLiteral(1)) => left
    // x + 0 = x
    case BinOp(left,           Add,      LoadLiteral(0)) => left
    case BinOp(LoadLiteral(0), Add,      right         ) => right
    // x - 0 = x
    case BinOp(left, Subtract, LoadLiteral(0)) => left
    // 0 / x = 0
    case BinOp(LoadLiteral(0), Divide, right) => LoadLiteral(0)

    // Non-simplifiable
    case _:ArrayAccess => expr
    case _:Call        => expr
    case _:BinOp       => expr
    case _:UnaryOp     => expr
    case _:Load        => expr
  }

  // Evaluate binary operations on compile-time known values.
  // TODO(miles): 64-bit int bounds checking.
  // TODO(miles): check edge cases, don't crash compiler. (example: divzero)
  private def evalBinOp(left: Long, op: BinOpType, right: Long): Long = op match {
    case Add              => left + right
    case Subtract         => left - right
    case Multiply         => left * right
    case Divide           => left / right
    case Mod              => left % right
    case LessThan         => if (left < right) 1 else 0
    case GreaterThan      => if (left > right) 1 else 0
    case LessThanEqual    => if (left <= right) 1 else 0
    case GreaterThanEqual => if (left >= right) 1 else 0
    case Equals           => if (left == right) 1 else 0
    case NotEquals        => if (left != right) 1 else 0
    case And              => if ((left != 0) && (right != 0)) 1 else 0
    case Or               => if ((left != 0) || (right != 0)) 1 else 0
  }
}
