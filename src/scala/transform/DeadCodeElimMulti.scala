package compile

/**
  * Applies Dead Code Elimination multiple times.
  */
object DeadCodeElimMulti extends Transformation {
  import IR2._

  override def transform(method: Method) : Method = {
    var newMethod = method
    for (_ <- 0 to 5) {
      newMethod = DeadCodeElim.transformMethod(newMethod)
    }
    return newMethod
  }

}
