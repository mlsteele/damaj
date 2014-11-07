package compile

object AvailableExpressions {
  import IR2._
  type T = Set[Expr]
}

class AvailableExpressions(override val method: IR2.Method) extends Analysis {
  import IR2._
  import ExprDependencies._

  type T = Set[Expr]

  private val allExprs:Set[Expr] = cfg.blocks.flatMap {
    _.stmts.flatMap {
      case Assignment(_, right) => List(right)
      case Call(_, args) => args.flatMap {
        case Left(_) => List()
        case Right(e) => List(e)
      }
      case Return(Some(e)) => List(e)
      case Return(None) => List()
    }
  }

  override def initial() = Set()

  override def bottom() = allExprs

  override def direction() = Forward

  override def merge(a: T, b: T) = a intersect b

  override def transfer(previous: T, block: Block) : T = {
    var avail: Set[Expr] = previous
    for (stmt: Statement <- block.stmts) stmt match {
      // Any expressions used by a statement are made available
      case Assignment(Store(to, index), right) => {
        val load = LoadField(to, index)
        // GEN expr
        avail += right

        // KILL any expressions that depended on the variable being assigned
        avail = avail.filter{ ! _.dependencies().contains(load) }
      }
      case c:Call => 
        // If the call has no side-effects, it can be an available expressio
        if (isPure(c)) { avail += c }
        // Also make its args available
        c.args.foreach {
          case Left(_) => // String, ignore
          case Right(e) =>  avail += e
        }
      case Return(ret) => ret.foreach{ e => avail += e}
    }
    return expungeGlobals(avail) filter {
      // make sure to not include any loads, generates unncessary temp vars
      case _:Load => false
      case _ => true
    }
  }

  /* Is this call pure i.e. safe to cache?
   * TODO actually evaluate purity of calls */
  private def isPure(c:Call):Boolean = false

  private def expungeGlobals(avail: Set[Expr]):Set[Expr] = {
    var newAvail: Set[Expr] = avail
    method.locals.globalTable.getFields.map{ global =>
      newAvail = newAvail.filter{ !_.dependencies().contains(LoadField(global, None)) }
    }
    newAvail
  }
}
