package compile

/*
 * After each block, this analysis returns the set of assignments that
 * reach (were not overwritten) after that block's execution.
 */
class ReachingDefinitions(override val method: IR2.Method) extends Analysis {
  import IR2._
  import SymbolTable._
  import ExprDependencies._

  type T = Set[Assignment]

  private val bottomVal = method.cfg.blocks.flatMap {b =>
    b.stmts.flatMap {
      case a:Assignment => List(a)
      case _ => List()
    }
  }.toSet

  def initial() = Set()

  def bottom() = bottomVal

  def direction() = Forward

  def merge(a:T, b:T) = a intersect b

  def transfer(previous: T, block: Block) : T = {
    var reaching: Set[Assignment] = previous
    for (stmt <- block.stmts) stmt match {
      case ass@Assignment(_,_) => {
        // Kill any assignments with the same store
        reaching = reaching filter { s => s.left != ass.left}
        // Kill any assignments whose args were modified by this assignment
        reaching = reaching filter { s => !(s.right.dependencies contains LoadField(ass.left)) }
        // Add this assignment to the reaching defs
        reaching += ass
      }
      case _:ArrayAssignment => // TODO: check if this matters or not
      case _:Call         => // don't care
      case _:Return       => // don't care
      case _:SpawnThreads => // don't care
    }
    return reaching filter {s => !isGlobal(s.left)}
  }

  private def isGlobal(field: FieldSymbol) : Boolean = method.params.globalTable.symbols contains field
}
