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
    val results = (new LiveVariables(method)).analyze()
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
      b.stmts.flatMap {
        // If an assignment contains a call, we can eliminate the
        // variable, but we can't eliminate the call, because it might
        // have side-effects
        case ass@Assignment(Store(to, index), call:Call) => {
          val variable = LoadField(to, None)
          (liveAfter(b) contains variable) match {
            case true => List(ass)
            case false => List(call)
          }
        }
        // If an assignment assigns to a dead var, no point in keeping it
        case ass@Assignment(Store(to, index), _) => {
          val variable = LoadField(to, index)
            (liveAfter(b) contains variable) match {
            case true => List(ass)
            case false => List()
          }
        }
        case c:Call => List(c)
        case r:Return => List(r)
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
