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

  def initial() = bottom()

  def bottom() = Set()

  def direction() = Forward

  def merge(a:T, b:T) = a union b

  def transfer(previous: T, block: Block) : T = {
    var reaching: Set[Assignment] = previous
    for (stmt <- block.stmts) stmt match {
      case ass@Assignment(_,_) => {
        // Kill any assignments with the same store
        reaching = reaching filter { s => s.left != ass.left}
        // Kill any assignments whose args were modified by this assignment
        reaching = reaching filter { s => !(s.right.dependencies contains storeToLoad(ass.left)) }
        // Add this assignment to the reaching defs
        reaching += ass
      }
      // If a call happens, assume that all global assigns were killed
      case _:Call => {
      }
      case _:Return => // don't care
    }
    return reaching filter {s => !isGlobal(s.left.to)}
  }

  private implicit def storeToLoad(store: Store) : LoadField = LoadField(store.to, store.index)

  private def isGlobal(field: FieldSymbol) : Boolean = method.params.globalTable.symbols contains field
}
