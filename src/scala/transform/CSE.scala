package compile

object CommonExpressionElimination extends Transformation {
  import IR2._
  import TempVarGen._

  override def transform(m: Method): Method =
    Method(m.id,
      m.params,
      m.locals,
      transform(m.cfg, new TempVarGen(m.locals, "~cse")),
      m.returnType)

  def transform(cfg: CFG, tempVarGen: TempVarGen): CFG = {
    new CSEHelper(cfg, tempVarGen).transformed
  }
}

// THIS HAS NEVER BEEN RUN AT ALL. PLEASE REMOVE THIS NOTICE WHEN IT'S BEEN VERIFIED A BIT.
// Do global subexpression elimination.
// Example Usage:
//   val cfg_awesome = new CSEHelper(cfg_lame, tempVarGen).transformed
class CSEHelper(cfg: CFG, tempVarGen: TempVarGen.TempVarGen) {
  import IR2._
  import TempVarGen._
  import SymbolTable._
  import AvailableExpressions.T
  type AnalysisResult = Analysis.AnalysisResult[T]

  private val analysis: AnalysisResult= new AvailableExpressions(cfg).analyze()

  private val carriers = new Carriers(tempVarGen)

  // Compute result. This is like a return value.
  val transformed: CFG = transform(cfg)

  private def debug(msg: String): Unit =
    Console.err.println(s"CSE $msg")

  private def transform(cfg: CFG): CFG = {
    cfg.mapBlocks(transform)
  }

  private def transform(block: Block): Block = {
    val avail = analysis.inputs(block)
    debug("considering %s avail:%s".format(block.uuid, avail))
    val statements = block.stmts.flatMap{ stmt =>
      transform(stmt, avail)
    }
    return Block(statements)
  }

  private def transform(stmt: Statement, available: T): List[Statement] = {
    debug(s"statement $stmt carriers:$carriers")
    stmt match {
      case Assignment(left, right) =>
        // Use stored expression if one is available according to the analysis,
        // (NOT according to carriers, which doesn't respect non-linear programs)
        // Otherwise, store computed value in case anyone wants to use it later.
        val assigns = available(right) match {
          case true =>
            // Load from carrier instead of doing calculation again.
            List(Assignment(left, carriers.load(right)))
          case false =>
            // Store for later and do original assignment.
            List(Assignment(carriers.store(right), right),
                 Assignment(left, carriers.load(right)))
        }

        // Recompute and store any available expressions that this assignment could
        // change the value of.
        val updates: List[Statement] = dependentExprs(available, left.asStore).map{ dependentExpr =>
          Assignment(carriers.store(dependentExpr), dependentExpr)
        }.toList

        assigns ++ updates
      case Return(Some(expr)) if available(expr) =>
        // Use stored expression if available.
        List(Return(Some(carriers.load(expr))))
      case _ =>
        // Leave all other types of assignment alone.
        List(stmt)
    }
  }

  // Extract the expressions that depend on a field.
  private def dependentExprs(exprs: Set[Expr], load: Load): Set[Expr] = {
    import ExprDependencies._
    exprs.filter(_.dependencies()(load))
  }

  implicit class EnhancedStore(store: Store) {
    def asStore: Load = store match {
      case Store(to, None) => LoadField(to, None)
      // Arrays not supported.
    }
  }

  // Helper that keeps track of which vars hold which exprs.
  private class Carriers(val tempVarGen: TempVarGen) {
    private var map = Map[Expr, FieldSymbol]()

    // Get the carrier var for an expression.
    // Allocate a spot if we haven't seen this yet.
    // This must do the allocation because the blocks might
    // be processed in any order.
    def of(expr: Expr): FieldSymbol = map.get(expr) match {
      case Some(carrier) => carrier
      case None          => allocate(expr)
    }

    // Get a load for the carried version of an expr.
    def load (expr: Expr): LoadField = LoadField(of(expr), None)
    // Get a store for the carrier of an expr.
    def store(expr: Expr): Store = Store(of(expr), None)

    // Introduce a new carrier variable.
    private def allocate(expr: Expr): FieldSymbol = {
      debug(s"allocating for $expr")
      assert(!map.contains(expr), s"cannot allocate for same expression twice: $expr")
      val carrier = tempVarGen.newIntVar()
      map += (expr -> carrier)
      carrier
    }

    override def toString: String = {
      map.toString
    }
  }
}
