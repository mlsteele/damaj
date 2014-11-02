package compile

/**
  * Utilities for analyzing what variables a expression depends on.
  */
object ExprDependencies {
  import IR2._

  implicit class BetterExpr(e: Expr) {
    // Set of loads that this expression depends on
    def dependencies(): Set[Load] = e match {
      case Call(_, args) => {
        var deps : Set[Load] = Set()
        args.foreach {
          case Left(_) =>
          case Right(subE) => deps = deps union subE.dependencies()
        }
        deps
      }
      case BinOp(left, _, right) => Set(left, right)
      case UnaryOp(_, right) => Set(right)
      case Ternary(cond, left, right) => Set(cond, left, right)
      case LoadField(_, index) => index.map(_.dependencies()).getOrElse(Set())
      case _:LoadLiteral => Set()
    }
  }
}
