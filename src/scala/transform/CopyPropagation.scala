package compile

object CopyPropagation {
  import IR2._
  import SymbolTable.FieldSymbol

  def apply(program: Program): Program = {
    val newMethods = program.methods.map(transformMethod _)
    val newMain = transformMethod(program.main)
    return Program(
      program.fields,
      newMain,
      newMethods
    )
  }

  def transformMethod(method: Method) : Method = {
    val results = (new ReachingDefinitions(method)).analyze()
    val reachingBefore = results.inputs
    val reachingAfter = results.outputs

    // Pretty print analysis results
    def annotate(b: Block) : String = {
      val inputsTitle = "===========\nREACHING BEFORE\n===========\n"
      val inputsString = reachingBefore(b).mkString("\n") + "\n"
      val outputsTitle = "===========\nREACHING AFTER\n===========\n"
      val outputsString = reachingAfter(b).mkString("\n")
      return inputsTitle + inputsString + outputsTitle + outputsString
    }

    Grapher.graph(method, "copyprop.reaching", Some(annotate(_)))

    val newCFG = method.cfg.mapBlocks { b =>
      b.stmts.flatMap {
        s => List(s)
      }
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
