package compile

object GlobalToLocal extends Transformation {
  import IR2._
  import SymbolTable._
  import ExprDependencies._

  override def apply(ir: Program):Program = {
    // Figures out which globals are used in which methods
    val globalsUsed : Map[Method, Set[FieldSymbol]] = (ir.main :: ir.methods).map {m => m -> globalsUsedByMethod(m)}.toMap

    // Figures out what methods used each global
    var globalsUsedInverse : Map[FieldSymbol, Set[Method]] = Map().withDefaultValue(Set())
    globalsUsed.keys.foreach {m =>
      globalsUsed(m).foreach { global =>
        globalsUsedInverse += global -> (globalsUsedInverse(global) + m)
      }
    }

    println(globalsUsedInverse)

    return ir
  }

  private def globalsUsedByMethod(method: Method) : Set[FieldSymbol] = {
    var used: Set[FieldSymbol] = Set()
    def maybeAddLoad(load: Load) : Unit = load match {
      case _:LoadLiteral =>
      case LoadField(from) => maybeAddField(from)
    }
    def maybeAddField(field: FieldSymbol) : Unit = method.locals.varOffset(field.id) match {
        case _:GlobalOffset => used += field; ()
        case _ =>
    }

    method.cfg.blocks.foreach { b =>
      b.stmts.foreach {
        case Assignment(left, right) => {
          maybeAddField(left)
          right.dependencies().foreach(maybeAddLoad)
        }
        case ArrayAssignment(left, index, right) => {
          maybeAddField(left)
          maybeAddLoad(index)
          maybeAddLoad(right)
        }
        case Call(_, args) => args.foreach {
          case Left(_) =>
          case Right(arg) => maybeAddLoad(arg)
        }
        case Return(ret) => ret.foreach(maybeAddLoad)
      }
    }
    return used
  }
}

