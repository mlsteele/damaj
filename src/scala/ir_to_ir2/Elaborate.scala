package compile

/**
  * Adds in explicit assignments to zero / false of all variables.
  * Converts void main() into int main() that returns 0.
  */
object Elaborate {
  import IR._
  import IRShared._
  import SymbolTable._

  def elaborate(program: ProgramIR) : ProgramIR = {
    // Mutates MethodSymbol's
    program.symbols.getMethods.foreach(_.elaborate())
    return program
  }

  // Generate a statement to assign a simple field to a 0 or false
  // If the field is a list, generate a statement to initialize each position to 0 or false
  def initVar(sym: Symbol) : List[Statement]= sym match {
    case f:FieldSymbol => f.size match {
      case None => List(
        Assignment(
          Store(f, None),
          if (f.dtype == DTInt) LoadInt(0) else LoadBool(false)
        )
      )
      case Some(length) => (0L to (length -1)).map { i =>
        Assignment(
          Store(f, Some(LoadInt(i))),
          if (f.dtype == DTInt) LoadInt(0) else LoadBool(false)
        )
      }.toList
    }
    case _ => List()
  }

  private implicit class ElaboratedMethodSymbol(method: MethodSymbol) {
    // Modifies void methods into methods that return int 0.
    def elaborate(): Unit = {
      val isVoidMain = (method.id == "main") && (method.returns == DTVoid)
      isVoidMain match {
        case true  =>
          method.block = method.block.elaborate().addReturnZero()
          method.returns = DTInt
        case false =>
          method.block = method.block.elaborate()
      }
    }
  }

  private implicit class ElaboratedBlock(block: Block) {
    def elaborate(): Block = {
      val newStmts = block.fields.symbols.flatMap(initVar)
      return Block(
        newStmts ++ block.stmts.map(_.elaborate),
        block.fields
      )
    }

    def addReturnZero(): Block = {
      val returnZero = Return(Some(LoadInt(0L)))
      return Block(
        block.stmts :+ returnZero,
        block.fields
      )
    }
  }

  private implicit class ElaboratedStatement(stmt: Statement) {
    def elaborate(): Statement = stmt match {
      case a:Assignment => a
      case c:Call => c
      case If(preStmts, cond, thenb, elseb) => If(
        preStmts.map(_.elaborate()),
        cond,
        thenb.elaborate(),
        elseb.map(_.elaborate())
      )
      case For(id, start, iter, thenb) => For(
        id,
        start,
        iter,
        thenb.elaborate()
      )
      case While(preStmts, cond, block, max) => While(
          preStmts.map(_.elaborate()),
          cond,
          block.elaborate(),
          max
        )
      case b:Break => b
      case c:Continue => c
      case r:Return => r
    }
  }
}
