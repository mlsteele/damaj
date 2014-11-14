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
    methods = program.methods.map(transformMethod),
    main = transformMethod(program.main)
  )

  def transformMethod(method: Method) : Method = {
    val newLocals = method.locals.copy()
    val tempGen = new TempVarGen(newLocals, "~inline")
    // keeps track of which calls should be replaced with what CFG
    var blockToCFG: Map[Block, CFG] = Map()
    method.cfg.blocks.foreach { b =>
      b.stmts.foreach {
        // If it's an inlineable method call, add this inlined cfg to our map
        case c:Call if c.isMethodCall() && c.method.isInlineable() => {
          val inlinedCFG = inlineCall(c, tempGen, None)
          Grapher.graph(method.copy(cfg=inlinedCFG), "%s.inliner.%s-call.inlined".format(method.id, c.method.id))
          blockToCFG += b -> inlinedCFG
        }
        case Assignment(to, c:Call) if c.isMethodCall() && c.method.isInlineable() => {
          val inlinedCFG = inlineCall(c, tempGen, Some(to))
          Grapher.graph(method.copy(cfg=inlinedCFG), "%s.inliner.%s-call.inlined".format(method.id, c.method.id))
          blockToCFG += b -> inlinedCFG
        }
        case _ => 
      }
    }
    // Replace all of the edges leading out of the original block with an edge leading out of the inlined CFG's start
    var newEdges = method.cfg.edges
    newEdges.foreach {case (b, e) => blockToCFG.get(b) match {
      case Some(inlinedCFG) => {
        newEdges -= b
        newEdges += inlinedCFG.start -> e
        newEdges ++= inlinedCFG.edges
      }
      case None => //not a call, don't replace
    }}

    // Replace all edges leading TO the original block with an edge leading to the inlined CFG's start
    newEdges = newEdges.mapValues {
      case e@Edge(toBlock) => blockToCFG.get(toBlock) match {
        case Some(inlinedCFG) => Edge(inlinedCFG.start)
        case None => e
      }
      case f@Fork(cond, leftBlock, rightBlock) => {
        val (newLeft, newRight) = (blockToCFG.get(leftBlock).map(_.start), blockToCFG.get(rightBlock).map(_.start))
        Fork(cond, newLeft.getOrElse(leftBlock), newRight.getOrElse(rightBlock))
      }
    }

    val newCFG = method.cfg
    return Method(
      method.id,
      method.params,
      newLocals,
      new CFG(newCFG.start, newCFG.end, newEdges),
      method.returnType
    )
  }

  // Returns the CFG to insert in place of the call
  def inlineCall(call: Call, tempGen: TempVarGen, result: Option[FieldSymbol]) : CFG = {
    val oldFields = call.method.locals.getFields
    var cfg = call.method.cfg
    // For each occurence of a variable in the original method, replace it with a temp
    for (oldField <- oldFields) {
      val newVar = tempGen.newIntVar()
      cfg = renameVar(oldField, LoadField(newVar), cfg)
    }
    // For each method arg referenced in the original cfg, replace it with the load passed to the call
    for (i <- (0 to call.args.length - 1)) call.args(i) match {
      case Left(_) => // this shouldn't happen
      case Right(load) => {
        cfg = renameVar(call.method.params.getFields(i), load, cfg)
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
    cfg.mapBlocks { b =>
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
  }

  private implicit class InlineableMethod(method: Method) {
    def isInlineable(): Boolean = {
      method.cfg.blocks.foreach {
        _.stmts.foreach {
          case Call(id, args) if id == method.id => return false
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

}
