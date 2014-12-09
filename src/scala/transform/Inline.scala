package compile

object Inline {
  import IR2._

  // Do not inline methods with more than this many blocks.
  val blockNumberThreshold: Int = 128

  def apply(program: Program) : Program = {
    return (new Inline(program)).transformProgram()
  }
}

private class Inline(program: IR2.Program) {
  import Inline.blockNumberThreshold
  import IR2._
  import TempVarGen._
  import SymbolTable._
  import FunctionalUtils._

  def transformProgram() = program.copy(
    // delete any inlineable methods
    methods = program.methods.flatMap {m => m.isInlineable match {
      // Delete any methods which can be inlined
      case true => List()
      // Perform inline optimization on body of all other methods
      case false => List(transformMethod(m))
    }},
    main = transformMethod(program.main)
  )

  def transformMethod(method: Method) : Method = {
    // Find every method call, and possibly inline it
    method.cfg.blocks.foreach { b =>
      b.stmts.foreach {
        // If it's an inlineable method call, add this inlined cfg to our map
        case c:Call if c.isMethodCall() && c.method.isInlineable() => {
          val newLocals = method.locals.copy()
          val tempGen = new TempVarGen(newLocals, "~%s_inline".format(c.id))
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
          val newLocals = method.locals.copy()
          val tempGen = new TempVarGen(newLocals, "~%s_inline".format(c.id))
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

    // Add an explicit edge from each return statement to the end block,
    // unless the block is the cfg's end
    cfg ++= CFG.fromBlock(CFG.nopBlock())
    val returnEdges = cfg.blocks.flatMap {
      b => b.stmts.flatMap {
        case r:Return if b != cfg.end => List((b, Edge(cfg.end)))
        case _                        => List()
      }
    }
    cfg = new CFG(cfg.start, cfg.end, cfg.edges ++ returnEdges)

    // Replace all returns with an assignment to the result variable
    result match {
      case None =>
      case Some(resultVar) => cfg = cfg.mapBlocks { b =>
        Block(b.stmts.map {
          case Return(Some(ret)) => Assignment(resultVar, ret)
          case s:Statement => s
        }, b.loopHead)
      }
    }
    return cfg
  }

  def renameVar(oldField: FieldSymbol, newLoad: Load, cfg: CFG) : CFG = {
    def rload(load: Load) : Load = load match {
      case LoadField(field) if field.id == oldField.id => newLoad
      case l:Load => l
    }
    def rfield(field: FieldSymbol) : FieldSymbol = field.id == oldField.id match {
      case true => newLoad.asInstanceOf[LoadField].from
      case false => field
    }
    val newCFG = cfg.mapBlocks { b =>
      Block(b.stmts.map {
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
        case s:SpawnThreads => s
      }, b.loopHead)
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
      val valid = all(method.cfg.blocks.map{
        _.stmts.map {
          case Call(id, args) if id == method.id => false
          case Assignment(_, Call(id, args)) if id == method.id => false
          case _ => true
        }
      }.flatten)
      val aGoodIdea = (method.cfg.blocks.size < blockNumberThreshold)
      return valid && aGoodIdea
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
