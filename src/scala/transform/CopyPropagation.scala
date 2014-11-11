package compile

object CopyPropagation {
  import IR2._
  import FunctionalUtils._

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

    // Use reaching definitions info to replace all loads occuring in statements with earlier-defined loads
    val newCFG = method.cfg.mapBlocks { b =>
      val fcc = followCopyChain(method, reachingBefore(b), _:Load)
      def fccCallArg(e: Expr) = fcc(e.asInstanceOf[Load])
      b.stmts.map {
        case Assignment(store, right) => right match {
          case load:Load => Assignment(store, fcc(load))
          case BinOp(left, op, right) => Assignment(store, BinOp(fcc(left), op, fcc(right)))
          case UnaryOp(op, right) => Assignment(store, UnaryOp(op, fcc(right)))
          case Call(id, args) => Assignment(store, Call(id, args.map(_.map(fccCallArg))))
        }
        case Call(id, args) => Call(id, args.map(_.map(fccCallArg)))
        case Return(ret) => Return(ret.map(fcc))
      }
    }

    return Method(method.id,
      method.params,
      method.locals,
      newCFG,
      method.returnType
    )
  }

  // Follows the trail of reaching definitions to try to replace the
  // given load with an earlier defined variable or constant
  private def followCopyChain(method: Method, reachingDefs: Set[Assignment], load: Load) : Load = load match {
    // Copy propagation makes no sense on literals
    case _:LoadLiteral => load
    // We're not going to open that can of worms
    case LoadField(_, Some(_)) => load

    case LoadField(from, None) => {
      val assignsThis = reachingDefs.filter {ass => ass.left == Store(from, None)}
      // TODO: if they all assign to the same constant, then we can use that constant
      // TODO: if they all assign to the same var, that might be okay?
      if (assignsThis.size != 1) return load
      // Only proceed if there is a single statement that can possibly assign to this var
      assignsThis.head.right match {
        // Nope, still not gonna open that can of worms
        case LoadField(_, Some(_)) => load
        case constant:LoadLiteral => constant
        case otherVar:LoadField   => method.params.lookupSymbolLocal(otherVar.from.id) match {
          // If the replaced var is a method arg, we can't replace it, because of a quirk in AsmGen
          case Some(_) => load
          case None => followCopyChain(method, reachingDefs, otherVar)
        }
        // Not a load, don't do copy propagation
        case _ => load
      }
    }
  }

  private implicit def stmtsToBlock(stmts: List[Statement]) : Block = Block(stmts)
}
