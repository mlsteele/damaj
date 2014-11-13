package compile

/**
  * Analyzes which variables are alive (still needed by later
  * statements) at each point of the method.  The return value of
  * analyze() returns a mapping from Blocks to the variables needed
  * BEFORE the block executes.
  */
class LiveVariables(override val method: IR2.Method) extends Analysis {
  import IR2._
  import ExprDependencies._
  import SymbolTable.FieldSymbol

  type T = Set[LoadField]

  // All global variables and all global array locations
  val globals = method.locals.globalTable.getFields.flatMap {
    case from@FieldSymbol(_, _, None) => List(LoadField(from))
    case _ => List()
  }

  def initial() = bottom()

  def bottom() = globals.toSet

  def direction() = Backward

  def merge(a: T, b: T) : T = a union b

  def transfer(previous: T, block: Block) : T = {
    var live:Set[LoadField] = previous
    for (stmt <- block.stmts) stmt match {
      // y = x; kills y, generates x
      case Assignment(to, right) => {
        // Remove any occurences of this variable from the live vars
        val killedLoad = LoadField(to)
        live -= killedLoad

        // Add any dependencies from the right hand side
        live ++= right.dependencies()

      }
      case ArrayAssignment(_, index:LoadField, right) => {
        live ++= index.dependencies()
        live ++= right.dependencies()
      }
      case ArrayAssignment(_, _:LoadLiteral, right) => {
        live ++= right.dependencies()
      }

      case Call(_, args) => args.foreach {
        case Left(_) =>
        case Right(e) => live ++= e.dependencies()
      }
      case Return(ret) => ret.foreach{e => live ++= e.dependencies}
    }
    // Special edge case: if this block is a child of a fork, the condition var needs
    // to be live
    cfg.edges.values.foreach {
      case Fork(cond:LoadField, left, right) if (left == block || right == block) =>
        live += cond
      case _ => 
    }
    // make sure to keep globals alive
    return live ++ globals.toSet
  }
}
