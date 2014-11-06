package compile

/**
  * Analyzes which statements are reachable.  The outputs represent
  * whether statements after a given block are reachable
  */
class Reachable(override val method: IR2.Method) extends Analysis {
  import IR2._

  type T = Boolean

  def initial() = true

  def bottom() = false

  def direction() = Forward

  def merge(a: T, b: T) : T = a || b

  def transfer(previous: T, block: Block) : T = {
    // If the parents are unreachable, then any statement afterward is unreachable
    if (previous == false) return false
    // If this block has a return, then any blocks afterwards are unreachable
    val hasReturn: Boolean = block.stmts.exists {
      case _:Return => true
      case _ => false
    }
    return !hasReturn
  }
}
