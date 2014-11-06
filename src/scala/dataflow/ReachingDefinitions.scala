package compile

/*
 * After each block, this analysis returns the set of assignments that
 * reach (were not overwritten) after that block's execution.
 */
class ReachingDefinitions(override val method: IR2.Method) extends Analysis {
  import IR2._

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
        // Add this assignment to the reaching defs
        reaching += ass
      }
      case _:Call => // don't care
      case _:Return => // don't care
    }
    return reaching
  }
}
