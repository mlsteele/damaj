package compile

// object CommonExpressionElimination extends Transformation {
//   import IR2._
//   import TempVarGen._

//   override def transform(m: Method): Method = {
//     val tempVarGen = new TempVarGen(m.locals, "~cse")
//     val newCFG = new CSEHelper(m, tempVarGen).transformed
//     return Uncondense(Method(m.id,
//       m.params,
//       m.locals,
//       newCFG,
//       m.returnType))
//   }
// }

// // Private helper for CommonExpressionElimination
// // Do global subexpression elimination.
// // Example Usage:
// //   val cfg_awesome = new CSEHelper(cfg_lame, tempVarGen).transformed
// class CSEHelper(method: IR2.Method, tempVarGen: TempVarGen.TempVarGen) {
//   import IR2._
//   import TempVarGen._
//   import SymbolTable._
//   import AvailableExpressions.T
//   type AnalysisResult = Analysis.AnalysisResult[T]

//   private val analysis: AnalysisResult = new AvailableExpressions(method).analyze()
//   Grapher.graph(method, "availexprs", Some(annotateGraph(_)))

//   // Carriers are the 'temps' which carry values around for reuse in CSE.
//   private val carriers = new Carriers(tempVarGen)

//   // Compute result. This is like a return value.
//   val transformed: CFG = transform(method.cfg)

//   private def annotateGraph(b: Block) : String = {
//     val title = "%s Output".format(b.uuid)
//     val contents = analysis.outputs(b).map{ expr =>
//       expr.toString
//     }.mkString(", ")
//     return s"$title\n{$contents}"
//   }

//   private def debug(msg: String): Unit = ()
//     // Console.err.println(s"CSE $msg")

//   private def transform(cfg: CFG): CFG = {
//     cfg.mapBlocks(transform)
//   }

//   private def transform(block: Block): Block = {
//     val avail = analysis.inputs(block)
//     debug("considering %s avail:%s".format(block.uuid, avail))
//     val statements = block.stmts.flatMap{ stmt =>
//       transform(stmt, avail)
//     }
//     return Block(statements)
//   }

//   private def transform(stmt: Statement, available: T): List[Statement] = {
//     debug(s"statement $stmt carriers:$carriers")
//     stmt match {
//       case Assignment(left, right) =>
//         val se = speedyExpr(right, available)
//         se.setup.toList :+ Assignment(left, se.load)
//       case Call(target, args) =>
//         val setups = args.flatMap{
//           case Left(strlit) => None
//           case Right(expr)  => speedyExpr(expr, available).setup
//         }

//         val newArgs = args.map{
//           case Left(strlit) => Left(strlit)
//           case Right(expr) => Right(speedyExpr(expr, available).load)
//         }

//         setups :+ Call(target, newArgs)
//       case Return(Some(expr)) =>
//         val se = speedyExpr(expr, available)
//         se.setup.toList :+ Return(Some(se.load))
//       case r@Return(None) => List(r)
//     }
//   }

//   // Represents a variable which might be accessed through a carrier.
//   // setup might include an assignment to save the value into a carrier for later.
//   // setup must happen right before expr is valid.
//   case class SpeedyExpr(setup: Option[Statement], load: LoadField)

//   // Use stored expression if one is available according to the analysis,
//   // (NOT according to carriers, which doesn't respect non-linear programs)
//   // Otherwise, store computed value in case anyone wants to use it later.
//   private def speedyExpr(expr: Expr, available: T): SpeedyExpr = available(expr) match {
//     case true =>
//       // Load from carrier instead of doing calculation again.
//       SpeedyExpr(None, carriers.load(expr))
//     case false =>
//       // Store for later and do original assignment.
//       val setup = Assignment(carriers.store(expr), expr)
//       SpeedyExpr(Some(setup), carriers.load(expr))
//   }

//   // Helper that keeps track of which vars hold which exprs.
//   private class Carriers(val tempVarGen: TempVarGen) {
//     private var map = Map[Expr, FieldSymbol]()

//     // Get the carrier var for an expression.
//     // Allocate a spot if we haven't seen this yet.
//     // This must do the allocation because the blocks might
//     // be processed in any order.
//     def of(expr: Expr): FieldSymbol = map.get(expr) match {
//       case Some(carrier) => carrier
//       case None          => allocate(expr)
//     }

//     // Get a load for the carried version of an expr.
//     def load (expr: Expr): LoadField = LoadField(of(expr), None)
//     // Get a store for the carrier of an expr.
//     def store(expr: Expr): Store = Store(of(expr), None)

//     // Introduce a new carrier variable.
//     private def allocate(expr: Expr): FieldSymbol = {
//       debug(s"allocating for $expr")
//       assert(!map.contains(expr), s"cannot allocate for same expression twice: $expr")
//       val carrier = tempVarGen.newIntVar()
//       map += (expr -> carrier)
//       carrier
//     }

//     override def toString: String = {
//       val els = map.map{
//         case (k,v) => s"k -> %s".format(v.id)
//       }.toList
//       "{%s}".format(els.mkString(", "))
//     }
//   }
// }
