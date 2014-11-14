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
      }
    }
    val newCFG = method.cfg
    return Method(
      method.id,
      method.params,
      newLocals,
      newCFG,
      method.returnType
    )
  }

  // Returns the CFG to insert in place of the call, and the field containing the return value
  def inlineCall(call: Call, tempGen: TempVarGen, result: Option[FieldSymbol]) : CFG = {
    val oldFields = call.method.locals.getFields
    var cfg = call.method.cfg
    // For each occurence of a variable in the original method, replace it with a temp
    for (oldField <- oldFields) {
      val newVar = tempGen.newIntVar()
      cfg = renameVar(oldField, newVar, cfg)
    }
    return cfg
  }

  def renameVar(oldField: FieldSymbol, newField: FieldSymbol, cfg: CFG) : CFG = {
    def rload(load: Load) : Load = load match {
      case LoadField(field) if field == oldField => LoadField(newField)
      case l:Load => l
    }
    def rfield(field: FieldSymbol) : FieldSymbol = field == oldField match {
      case true => newField
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
