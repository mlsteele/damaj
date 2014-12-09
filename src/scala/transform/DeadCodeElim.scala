package compile

object DeadCodeElim {
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
      Block(b.stmts.flatMap {
        // If an assignment contains a call, we can eliminate the
        // variable, but we can't eliminate the call, because it might
        // have side-effects
        case ass@Assignment(to, call:Call) => {
          val variable = LoadField(to)
          (liveAfter(b) contains variable) match {
            case true => List(ass)
            // Discard the assignment, but still do the function call
            case false => List(call)
          }
        }
        case ass@Assignment(to, expr:Expr) => {
          val variable = LoadField(to)
          // Don't kill the statement if it might have a side effect
          (liveAfter(b) contains variable) || expr.hasSideEffect match {
            case true => List(ass)
            case false => List()
          }
        }
        // Todo: open this can of worms
        case ass:ArrayAssignment => List(ass) //don't kill for now
        case c:Call => List(c)
        case r:Return => List(r)
        case s:SpawnThreads => List(s)
      }, b.loopHead)
    }

    return Method(method.id,
      method.params,
      method.locals,
      newCFG,
      method.returnType
    )
  }


  private implicit class PureExpr(e: Expr) {
    def hasSideEffect() : Boolean = e match {
      case _:Call => true
      // If the index is out of bounds, then this load has a side-effect
      case ArrayAccess(field, LoadLiteral(i)) => (i < 0) || (i >= field.size.get)
      // Can't tell if the index is variable. Assume it has a side-effect
      case _:ArrayAccess => true
      case _ => false
    }
  }
}
