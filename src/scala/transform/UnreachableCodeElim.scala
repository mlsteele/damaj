package compile

/**
  * Eliminates code that is unreachable.
  * For now, this only deletes statements after a return.
  * TODO: make this also remove branches with a constant condition
  */
object UnreachableCodeElim extends Transformation {
  import IR2._

  override def transform(method: Method) : Method = {
    val results = (new Reachable(method)).analyze()
    val reachableBefore = results.inputs
    val reachableAfter = results.outputs

    def annotate(b: Block) : String = {
      val inputsTitle = "===========\nREACHABLE BEFORE\n===========\n"
      val inputsString = reachableBefore(b) + "\n"
      val outputsTitle = "===========\nREACHABLE AFTER\n===========\n"
      val outputsString = reachableAfter(b) + "\n"
      return inputsTitle + inputsString + outputsTitle + outputsString
    }

    Grapher.graph(method, "unreachable.reachable", Some(annotate(_)))

    val newCFG = method.cfg.mapBlocks { b => 
      b.stmts.filter {s => reachableBefore(b)}
    }

    return Method(method.id,
      method.params,
      method.locals,
      newCFG,
      method.returnType
    )
  }

  private implicit def stmtsToBlock(stmts: List[Statement]) : Block = Block(stmts)
}
