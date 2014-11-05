package compile

// THIS HAS NEVER BEEN RUN AT ALL. PLEASE REMOVE THIS NOTICE WHEN IT'S BEEN VERIFIED A BIT.
// Do global subexpression elimination.
// Example Usage:
//   val cfg_awesome = new CommonExpressionElimination(cfg_lame, tempVarGen).transformed
class CommonExpressionElimination(cfg: CFG, tempVarGen: TempVarGen.TempVarGen) {
  import IR2._
  import TempVarGen._
  import SymbolTable._
  import AvailableExpressions.T
  type AnalysisResult = Analysis.AnalysisResult[T]

  private val analysis: AnalysisResult= new AvailableExpressions(cfg).analyze()

  private val carriers = new Carriers(tempVarGen)

  // Compute result. This is like a return value.
  val transformed: CFG = transform(cfg)

  private def transform(cfg: CFG): CFG = {
    // TODO(miles): actually do stuff.
    // cfg.mapBlocks(transform)
    transform(cfg.start) // This makes the warnings go away.
    return cfg
  }

  private def transform(block: Block): Block = {
    val statements = block.stmts.flatMap{ stmt =>
      transform(stmt, analysis.inputs(block))
    }
    return Block(statements, block.fields)
  }

  private def transform(stmt: Statement, available: T): List[Statement] = stmt match {
    case Assignment(left, right) =>
      // Use stored expression if one is available according to the analysis,
      // (NOT according to carriers, which doesn't respect non-linear programs)
      // Otherwise, store computed value in case anyone wants to use it later.
      val assign = available(right) match {
        case true =>
          Assignment(left, carriers.load(right))
        case false =>
          carriers.allocate(right)
          Assignment(carriers.store(right), right)
      }

      // Recompute and store any available expressions that this assignment could
      // change the value of.
      val updates: List[Statement] = dependentExprs(available, left.asStore).map{ dependentExpr =>
        Assignment(carriers.store(dependentExpr), dependentExpr)
      }.toList

      assign +: updates
    case _ =>
      // Leave all other types of assignment alone.
      List(stmt)
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

    // Introduce a new carrier variable.
    def allocate(expr: Expr): FieldSymbol = {
      assert(!map.contains(expr), s"cannot allocate for same expression twice: $expr")
      val carrier = tempVarGen.newIntVar()
      map += (expr -> carrier)
      carrier
    }

    // Get the carrier var for an expression.
    // expr must have been previously allocated.
    def of(expr: Expr): FieldSymbol = map(expr)

    def load (expr: Expr): LoadField = LoadField(of(expr), None)
    def store(expr: Expr): Store     = Store(of(expr), None)
  }
}
