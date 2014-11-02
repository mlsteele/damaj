package compile

/**
  * Analyzes which variables are alive (still needed by later
  * statements) at each point of the method.  The return value of
  * analyze() returns a mapping from Blocks to the variables needed
  * BEFORE the block executes.
  */
object LiveVariables extends Analysis {
  import IR2._
  type T = Set[Load]

  def bottom() = Set()

  def direction() = Backward

  def merge(a: T, b: T) : T = a union b

  def transfer(previous: T, block: Block) : T = {
    var live:Set[Load] = previous
    for (stmt <- block.stmts) stmt match {
      // Assignments kill the var on the left
      case Assignment(Store(to, index), right) => {
        // Add any dependencies from the right hand side
        live = live ++ right.dependencies()
        // Remove any occurences of this variable from the live vars
        val killedLoad = LoadField(to, index)
        live = live - killedLoad
      }
      case Call(_, args) => args.foreach {
        case Left(_) =>
        case Right(e) => live = live ++ e.dependencies()
      }
      case Return(ret) => ret.foreach{e => live = live + e}
    }
    return live
  }

  //TODO: make this code not copy-pasta
  private implicit class BetterExpr(e: Expr) {
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
