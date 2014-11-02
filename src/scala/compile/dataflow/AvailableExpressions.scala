package compile

object AvailableExpressions extends Analysis {
  import IR2._
  import ExprDependencies._

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


}
