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

    val newEdges = method.cfg.edges.mapValues {
      case f@Fork(cond, left, right) => {
        // Find the block leading to this fork
        val prevBlock = method.cfg.edges.find(_._2 == f).get._1
        val fcc = followCopyChain(method, reachingAfter(prevBlock), _:Load)
        // Replace the block's condition
        Fork(fcc(cond), left, right)
      }
      case e:Edge => e
    }

    // Use reaching definitions info to replace all loads occuring in statements with earlier-defined loads
    val newCFG = (new CFG(method.cfg.start, method.cfg.end, newEdges)).mapBlocks { b =>
      val fcc = followCopyChain(method, reachingBefore(b), _:Load)
      val fccArg = followCopyChain(method, reachingBefore(b), _:Load, true)
      Block(b.stmts.map {
        case Assignment(field, right) => right match {
          case load:Load                            => Assignment(field, fcc(load))
          case ArrayAccess(arrayField, index)       => Assignment(field, ArrayAccess(arrayField, fcc(index)))
          case BinOp(left, op, right)               => Assignment(field, BinOp(fcc(left), op, fcc(right)))
          case UnaryOp(op, right)                   => Assignment(field, UnaryOp(op, fcc(right)))
          case Call(id, args)                       => Assignment(field, Call(id, args.map(_.map(fccArg))))
        }
        case ArrayAssignment(field, index, right) => ArrayAssignment(field, fcc(index), fcc(right))
        case Call(id, args) => Call(id, args.map(_.map(fccArg)))
        case Return(ret) => Return(ret.map(fcc))
      }, b.loopHead)
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
  private def followCopyChain(method: Method, reachingDefs: Set[Assignment], load: Load, isCallArg: Boolean = false) : Load = load match {
    // Copy propagation ends when you reach a literal
    case _:LoadLiteral => load
    case LoadField(from) => {
      val assignsThis = reachingDefs.filter {ass => ass.left == from}
      // TODO: if they all assign to the same constant, then we can use that constant
      // TODO: if they all assign to the same var, that might be okay?
      if (assignsThis.size != 1) return load
      // Only proceed if there is a single statement that can possibly assign to this var
      assignsThis.head.right match {
        case constant:LoadLiteral => constant
          // If otherVar is a parameter, we still need the local load in case this load will be passed to another function
        case otherVar:LoadField   => isCallArg && method.params.symbols.contains(otherVar.from) match {
          case true => load
          case false => followCopyChain(method, reachingDefs, otherVar)
        }
        // Not a load, don't do copy propagation
        case _ => load
      }
    }
  }
}
