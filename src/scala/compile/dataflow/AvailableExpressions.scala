package compile

object AvailableExpressions extends Analysis {
  import IR2._
  import IRShared.StrLiteral

  type T = Set[Expr]

  override def bottom() = Set()

  override def direction() = Forward

  override def merge(a: T, b: T) = a intersect b

  override def transfer(previous: T, block: Block) : T = {
    var avail: Set[Expr] = previous
    for (stmt: Statement <- block.stmts) stmt match {
      // Any expressions used by a statement are made available
      case Assignment(Store(to, index), right) => {
        val load = LoadField(to, index)
        avail = avail + right
        // Remove any expressions that depended on the variable being assigned to
        avail = avail.filter {e => ! (e.dependencies() contains load)}
      }
      case Call(_, args) => args.foreach {
        case Left(_) =>
        case Right(e) => avail = avail + e
      }
      case Return(ret) => ret.foreach{e => avail = avail + e}
    }
    return avail
  }

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
