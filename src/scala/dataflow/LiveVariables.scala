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

  type T = Set[Load]

  val globals = method.locals.globalTable.getFields.map( LoadField(_, None))

  def initial() = bottom()

  def bottom() = globals.toSet

  def direction() = Backward

  def merge(a: T, b: T) : T = a union b

  def transfer(previous: T, block: Block) : T = {
    var live:Set[Load] = previous
    for (stmt <- block.stmts) stmt match {
      // Assignments kill the var on the left
      case Assignment(Store(to, _), right) => {
        // Remove any occurences of this variable from the live vars
        // We treat arrays as a single variable, effectively killing the entire array
        // We also make sure to always treat globals as live
        val killedLoad = LoadField(to, None)
        if (!( globals contains killedLoad)) {
          live -= killedLoad
        }

        // Add any dependencies from the right hand side
        live ++= right.dependencies()

      }
      case Call(_, args) => args.foreach {
        case Left(_) =>
        case Right(e) => live ++= e.dependencies()
      }
      case Return(ret) => ret.foreach{e => live += e}
    }
    // Special edge case: if this block is a child of a fork, the condition var needs
    // to be live
    cfg.edges.values.foreach {
      case Fork(cond, left, right) if (left == block || right == block) =>
        live += cond
      case _ => 
    }
    return live
  }
}
