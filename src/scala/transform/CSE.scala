package compile

object CommonExpressionElimination extends Transformation {
  import IR2._
  import TempVarGen._

  override def transform(m: Method): Method = {
    val tempVarGen = new TempVarGen(m.locals, "~cse")
    val newCFG = new CSEHelper(m, tempVarGen).transformed
    return Uncondense(Method(m.id,
      m.params,
      m.locals,
      newCFG,
      m.returnType))
  }
}

// Private helper for CommonExpressionElimination
// Do global subexpression elimination.
// Example Usage:
//   val cfg_awesome = new CSEHelper(cfg_lame, tempVarGen).transformed
class CSEHelper(method: IR2.Method, tempVarGen: TempVarGen.TempVarGen) {
  import IR2._
  import TempVarGen._
  import SymbolTable._
  import AvailableExpressions.T
  type AnalysisResult = Analysis.AnalysisResult[T]

  private val analysis: AnalysisResult = new AvailableExpressions(method).analyze()
  Grapher.graph(method, "availexprs", Some(annotateGraph(_)))

  // Carriers are the 'temps' which carry values around for reuse in CSE.
  private val carriers = new Carriers(tempVarGen)

  // Compute result. This is like a return value.
  val transformed: CFG = transform(method.cfg)

  private def annotateGraph(b: Block) : String = {
    val title = "%s Output".format(SlugGenerator.id(b))
    val contents = analysis.outputs(b).map{ expr =>
      expr.toString
    }.mkString(", ")
    return s"$title\n{$contents}"
  }

  private def debug(msg: String): Unit = ()
    // Console.err.println(s"CSE $msg")

  private def transform(cfg: CFG): CFG = {
    cfg.mapBlocks(transform)
  }

  private def transform(block: Block): Block = {
    val avail = analysis.inputs(block)
    debug("considering %s avail:%s".format(SlugGenerator.id(block), avail))
    val statements = block.stmts.flatMap{ stmt =>
      transform(stmt, avail)
    }
    return Block(statements)
  }

  private def transform(stmt: Statement, available: T): List[Statement] = {
    debug(s"statement $stmt carriers:$carriers")
    stmt match {
      case a@Assignment(left, right) =>
        val se = speedyExpr(right, available)
        val post = se.storeTo.toList.map{
          f => Assignment(f, LoadField(left))
        }
        a.copy(right = se.use) +: post.toList

      case a@ArrayAssignment(left, index, right) =>
        val se = speedyExpr(right, available)
        val post = se.storeTo.map{ f =>
          ArrayAssignment(f, index, LoadField(left))
        }
        // Dangerous cast se.use to a Load.
        // This is 'safe' because `right` with which `se` was built was a Load,
        // and speedyExpr does not return more complicated expressions.
        a.copy(right = se.use.asInstanceOf[Load]) +: post.toList

      // Calls have only Load arguments (and strings), so would not benefit from CSE.
      case c:Call => List(c)
      // Return return only a Load, so would not benefit from CSE.
      case r:Return => List(r)
    }
  }

  // Represents a variable which might be accessed through a carrier.
  // post might be an assignment to save the value into a carrier for later.
  // post MUST happen right after expr is valid.
  case class SpeedyExpr(use: Expr, storeTo: Option[FieldSymbol])

  // Use stored expression if one is available according to the analysis,
  // (NOT according to carriers, which doesn't respect non-linear programs)
  // Otherwise, store computed value in carrier in case anyone wants to use it later.
  private def speedyExpr(expr: Expr, available: T): SpeedyExpr =
    (expr, available(expr)) match {
      case (expr:Load, _) =>
        // Don't bother loading Loads from carriers. That wouldn't be any speedier.
        SpeedyExpr(expr, None)
      case (_, true) =>
        // Load from carrier instead of doing calculation again.
        SpeedyExpr(carriers.load(expr), None)
      case (_, false) =>
        // Output original expression, but demand it be stored in a carrier after.
        SpeedyExpr(expr, Some(carriers.of(expr)))
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
    def load (expr: Expr): LoadField = LoadField(of(expr))

    // Introduce a new carrier variable.
    private def allocate(expr: Expr): FieldSymbol = {
      debug(s"allocating for $expr")
      assert(!map.contains(expr), s"cannot allocate for same expression twice: $expr")
      val carrier = tempVarGen.newIntVar()
      map += (expr -> carrier)
      carrier
    }

    override def toString: String = {
      val els = map.map{
        case (k,v) => s"k -> %s".format(v.id)
      }.toList
      "{%s}".format(els.mkString(", "))
    }
  }
}
