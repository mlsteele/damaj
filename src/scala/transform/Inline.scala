package compile

object Inline {
  import IR2._
  def apply(program: Program) : Program = {
    return (new Inline(program)).transformProgram()
  }
}

private class Inline(program: IR2.Program) {
  import IR2._
  import TempVarGen._
  import SymbolTable._
  import FunctionalUtils._

  def transformProgram() = program.copy(
    // delete any inlineable methods
    methods = program.methods.filter {m => !m.isInlineable},
    main = transformMethod(program.main)
  )

  def transformMethod(method: Method) : Method = {
    val newLocals = method.locals.copy()
    val tempGen = new TempVarGen(newLocals, "~inline")
    // Find every method call, and possibly inline it
    method.cfg.blocks.foreach { b =>
      b.stmts.foreach {
        // If it's an inlineable method call, add this inlined cfg to our map
        case c:Call if c.isMethodCall() && c.method.isInlineable() => {
          val inlinedCFG = inlineCall(c, tempGen, None)
          Grapher.graph(method.copy(cfg=inlinedCFG), "%s.inliner.%s-call.inlined".format(method.id, c.method.id))
          return transformMethod(Method(
            method.id,
            method.params,
            newLocals,
            method.cfg.replaceBlock(b, inlinedCFG),
            method.returnType
          ))
        }
        case Assignment(to, c:Call) if c.isMethodCall() && c.method.isInlineable() => {
          val inlinedCFG = inlineCall(c, tempGen, Some(to))
          Grapher.graph(method.copy(cfg=inlinedCFG), "%s.inliner.%s-call.inlined".format(method.id, c.method.id))
          return transformMethod(Method(
            method.id,
            method.params,
            newLocals,
            method.cfg.replaceBlock(b, inlinedCFG),
            method.returnType
          ))
        }
        case _ => 
      }
    }
    return method
  }

  // Returns the CFG to insert in place of the call
  def inlineCall(call: Call, tempGen: TempVarGen, result: Option[FieldSymbol]) : CFG = {
    var cfg = call.method.cfg
    // For each occurence of a variable in the original method, replace it with a temp
    for (oldField <- call.method.locals.getFields) {
      val newVar = tempGen.newVarLike(oldField)
      cfg = renameVar(oldField, LoadField(newVar), cfg)
    }
    // Replace all references to a parameter with a temporary
    for ((param, i) <- call.method.params.getFields.zipWithIndex) {
      // Generate temp for each parameter
      val newParamVar = tempGen.newVarLike(param)
      // Rename all references to original parameter
      cfg = renameVar(param, LoadField(newParamVar), cfg)
      // Insert code to initialize parameter temps with call args
      val callArg = call.args(i)
      callArg match {
        case Left(_) =>
        case Right(callLoad) => {
          val paramAssign = Assignment(newParamVar, callLoad)
          cfg = CFG.fromStatement(paramAssign) ++ cfg
        }
      }
    }

    // Replace all returns with an assignment to the result variable
    result match {
      case None =>
      case Some(resultVar) => cfg = cfg.mapBlocks { b =>
        b.stmts.map {
          case Return(Some(ret)) => Assignment(resultVar, ret)
          case s:Statement => s
        }
      }
    }
    return cfg
  }

  def renameVar(oldField: FieldSymbol, newLoad: Load, cfg: CFG) : CFG = {
    def rload(load: Load) : Load = load match {
      case LoadField(field) if field == oldField => newLoad
      case l:Load => l
    }
    def rfield(field: FieldSymbol) : FieldSymbol = field == oldField match {
      case true => newLoad.asInstanceOf[LoadField].from
      case false => field
    }
    val newCFG = cfg.mapBlocks { b =>
      b.stmts.map {
        case Assignment(to, expr) => {
          val newRight = expr match {
            case l:Load => rload(l)
            case ArrayAccess(arrayField, index) => ArrayAccess(rfield(arrayField), rload(index))
            case BinOp(left, op, right) => BinOp(rload(left), op, rload(right))
            case UnaryOp(op, right) => UnaryOp(op, rload(right))
            case Call(id, args) => Call(id, args.map(_.map(rload)))
          }
          Assignment(rfield(to), newRight)
        }
        case ArrayAssignment(to, index, right) => ArrayAssignment(rfield(to), rload(index), rload(right))
        case Call(id, args) => Call(id, args.map(_.map(rload)))
        case Return(ret) => Return(ret.map(rload))
      }
    }
    // Rename vars in forks too..
    val newEdges = newCFG.edges.mapValues {
      case Fork(cond, left, right) => Fork(rload(cond), left, right)
      case e:Edge => e
    }
    return new CFG(newCFG.start, newCFG.end, newEdges)
  }

  private implicit class InlineableMethod(method: Method) {
    def isInlineable(): Boolean = {
      method.cfg.blocks.foreach {
        _.stmts.foreach {
          case Call(id, args) if id == method.id => return false
          case Assignment(_, Call(id, args)) if id == method.id => return false
          case _ =>
        }
      }
      return true
    }
  }

  private implicit class CallMethod(call: Call) {
    def isMethodCall() : Boolean = program.methods.exists{ m =>
      m.id == call.id
    } || call.id == "main"

    def method() : Method = call.id match {
      case "main" => program.main
      case _ => program.methods.find(m => m.id == call.id).get
    }
  }

  private implicit class EnhancedCFG(cfg: CFG) {
    def replaceBlock(block: Block, inlined:CFG) : CFG = {
      var newEdges = cfg.edges ++ inlined.edges
      // Replace all edges leading IN to the original block with edges going to the inlined cfg's start
      newEdges = newEdges.mapValues {
        case Edge(to) => if (to == block) Edge(inlined.start) else Edge(to)
        case Fork(cond, left, right) => if (left == block && right == block) {
          Fork(cond, inlined.start, inlined.start)
        } else if (left == block) {
          Fork(cond, inlined.start, right)
        } else if (right == block) {
          Fork(cond, left, inlined.start)
        } else {
          Fork(cond, left, right)
        }
      }
      // Replace the edge leading OUT of the original block with an edge coming out of the inlined cfg's end
      newEdges = newEdges.map {case (b, e) => {
        if (b == block) (inlined.end, e) else (b, e)
      }}
      val newStart = if (cfg.start == block) inlined.start else cfg.start
      val newEnd = if (cfg.end == block) inlined.end else cfg.end
      new CFG(newStart, newEnd, newEdges)
    }
  }

}
