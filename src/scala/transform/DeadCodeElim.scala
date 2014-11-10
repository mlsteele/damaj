package compile

object DeadCodeElim {
  import IR2._
  import SymbolTable.FieldSymbol

  def apply(program: Program, runNumber: Int = 0): Program = {
    val newMethods = program.methods.map(transformMethod(_, runNumber))
    val newMain = transformMethod(program.main, runNumber)
    return Program(
      program.fields,
      newMain,
      newMethods
    )
  }

  def transformMethod(method: Method, runNumber: Int) : Method = {
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

    Grapher.graph(method, "deadcode-%d.live".format(runNumber), Some(annotate(_)))

    val newCFG = method.cfg.mapBlocks { b =>
      b.stmts.flatMap {
        // If an assignment contains a call, we can eliminate the
        // variable, but we can't eliminate the call, because it might
        // have side-effects
        case ass@Assignment(store@Store(to, index), call:Call) => {
          val variable = LoadField(to, index)
          // Don't kill the assignment if it's needed or has a side effect
          (liveAfter(b) contains variable) || store.hasSideEffect match {
            case true => List(ass)
            // Discard the assignment, but still do the function call
            case false => List(call)
          }
        }
        // If an assignment assigns to a dead var, no point in keeping it
        case ass@Assignment(store@Store(to, index), expr:Expr) => {
          val variable = LoadField(to, index)
          // Don't kill the statement if it might have a side effect
            (liveAfter(b) contains variable) || expr.hasSideEffect || store.hasSideEffect match {
            case true => List(ass)
            case false => List()
          }
        }
        case c:Call => List(c)
        case r:Return => List(r)
      }
    }

    val newLocals = method.locals.copy()
    val everAlive = liveBefore.values.fold(Set())(_ union _)
    newLocals.symbols.foreach {
      case field@FieldSymbol(_, _, None) if (!everAlive.contains(LoadField(field, None))) => newLocals.removeSymbol(field)
      case _ =>
    }
    return Method(method.id,
      method.params,
      newLocals,
      newCFG,
      method.returnType
    )
  }

  private implicit class PureStore(s: Store) {
    def hasSideEffect() : Boolean = s match {
      case Store(field, Some(LoadLiteral(i))) => (i < 0) || (i >= field.size.get)
      case Store(field, Some(_)) => true
      case Store(_, None) => false
    }
  }

  private implicit class PureExpr(e: Expr) {
    def hasSideEffect() : Boolean = e match {
      case _:Call => true
      // If the index is out of bounds, then this load has a side-effect
      case LoadField(field, Some(LoadLiteral(i))) => (i < 0) || (i >= field.size.get)
      // Can't tell if the index is variable. Assume it has a side-effect
      case LoadField(field, Some(_)) => true
      case _ => false
    }
  }

  private implicit def stmtsToBlock(stmts: List[Statement]) : Block = Block(stmts)
}
