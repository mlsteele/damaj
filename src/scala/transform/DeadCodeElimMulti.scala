package compile

/**
  * Applies Dead Code Elimination multiple times.
  */
object DeadCodeElimMulti extends Transformation {
  import IR2._

  override def transform(method: Method) : Method = {
    val prevSize = methodSize(method)
    val newMethod = DeadCodeElim.transformMethod(method)
    if (methodSize(newMethod) == prevSize)  {
      return newMethod
    }
    return transform(newMethod)
  }

  private def methodSize(method: Method) : Int = {
    method.cfg.blocks.map(_.stmts.length).foldLeft(0)(_ + _)
  }

}
