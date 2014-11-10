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
    val showStopper: Boolean = block.stmts.exists {
      case _:Return => true
      case Assignment(store, right) => storeIsInvalid(store) || exprIsInvalid(right)
      case _ => false
    }
    return !showStopper
  }

  def storeIsInvalid(store: Store): Boolean  = store match {
    case Store(field, Some(LoadLiteral(i))) => (i < 0) || (i >= field.size.get)
    case _ => false
  }

  def exprIsInvalid(expr: Expr): Boolean = expr match {
    case LoadField(field, i) => storeIsInvalid(Store(field, i))
    case _ => false
  }
}
