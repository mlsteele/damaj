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
    Block(block.stmts.map(transform), block.loopHead)

  def transform(stmt: Statement): Statement = stmt match {
    case x:Assignment      => x.copy(right = simplify(x.right))
    case _ => stmt
  }

  private def simplify(expr: Expr): Expr = expr match {
    // 3 * 5 = 15 // constant calculation
    case BinOp(LoadLiteral(x), op:BinOpType, LoadLiteral(y)) =>
      // Do the computation now, unless it would fail, in which case leave it for the program to fail.
      evalBinOp(x, op, y).map(LoadLiteral(_)).getOrElse(expr)
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
  // Returns None if the program would fail.
  // TODO(miles): 64-bit int bounds checking.
  // TODO(miles): check edge cases, don't crash compiler. (example: divzero)
  private def evalBinOp(left: Long, op: BinOpType, right: Long): Option[Long] = op match {
    case Add              => Some(left + right)
    case Subtract         => Some(left - right)
    case Multiply         => Some(left * right)
    case Divide           => if (right != 0) Some(left / right) else None
    case Mod              => Some(left % right)
    case LessThan         => if (left < right) Some(1) else Some(0)
    case GreaterThan      => if (left > right) Some(1) else Some(0)
    case LessThanEqual    => if (left <= right) Some(1) else Some(0)
    case GreaterThanEqual => if (left >= right) Some(1) else Some(0)
    case Equals           => if (left == right) Some(1) else Some(0)
    case NotEquals        => if (left != right) Some(1) else Some(0)
    case And              => if ((left != 0) && (right != 0)) Some(1) else Some(0)
    case Or               => if ((left != 0) || (right != 0)) Some(1) else Some(0)
  }
}
