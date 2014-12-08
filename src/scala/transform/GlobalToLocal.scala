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

    // Now, figure out which globals are only used by one method, and move it into that method's symbol table
    globalsUsedInverse.foreach { case (global, methods) =>
      if (methods.size == 1) {
        val method = methods.head
        // Remove it from the global symbol table
        method.locals.globalTable.removeSymbol(global)
        // Insert it into the method's symbol table
        method.locals.addSymbol(global)
      }
    }

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

