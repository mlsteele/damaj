package compile

object DeadCodeElim {
  import IR2._

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
    val results = (new LiveVariables(method.cfg)).analyze()
    val liveAfter = results.inputs
    val liveBefore = results.outputs

    // Pretty print analysis results
    def annotate(b: Block) : String = {
      val inputsTitle = "===========\nLIVE BEFORE\n===========\n"
      val inputsString = liveBefore(b).mkString("\n") + "\n"
      val outputsTitle = "===========\nLIVE AFTER\n===========\n"
      val outputsString = liveAfter(b).mkString("\n")
      return inputsTitle + inputsString + outputsTitle + outputsString
    }

    Grapher.graph(method, "deadcode.live", Some(annotate(_)))

    val newCFG = method.cfg.mapBlocks { b =>
      b.stmts.filter{
        case Assignment(Store(to, index), _) => {
          val variable = LoadField(to, index)
          // If this variable is not live after this block, there is no point in keeping this assignment
           liveAfter(b) contains variable
        }
        case c:Call => true
        case r:Return => true
      }
    }

    val newLocals = method.locals
    return Method(method.id,
      method.params,
      newLocals,
      newCFG,
      method.returnType
    )
  }

  private implicit def stmtsToBlock(stmts: List[Statement]) : Block = Block(stmts)
}
