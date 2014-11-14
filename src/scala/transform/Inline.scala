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

  def transformProgram() = program

  def transformMethod(method: Method) : Method = {
    val newLocals = method.locals.copy()
    val tempGen = new TempVarGen(newLocals)
    val newCFG = method.cfg.mapBlocks { b =>
      b.stmts.flatMap {
        case c:Call if c.method.isInlineable => {
          val inlinedCFG = inlineCall(c, tempGen)
          Grapher.graph(method, "%s.inliner.%s-call.inlined".format(method.id, c.method.id))
          List(c)
        }
        case s:Statement => List(s)
      }
    }
    return Method(
      method.id,
      method.params,
      newLocals,
      newCFG,
      method.returnType
    )
  }

  def inlineCall(call: Call, tempGen: TempVarGen) : CFG = {
    return new CFG(List(), List(), Map())
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
    def method() : Method = program.methods.find(m => m.id == call.id).get
  }

}
