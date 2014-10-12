package compile

// Example usage:
// println(IRPrinter.print(ir))
object IRPrinter {
  import IRShared._
  import IR._
  import SymbolTable._

  def print(ir:IR.ProgramIR): String = {
    lines(ir.symbols.symbols.map(printFullSymbol))
  }

  def printFullSymbol(symbol:Symbol): String = symbol match {
    case c:CalloutSymbol => "callout \"%s\"".format(c.id)
    case f:FieldSymbol => printField(f)
    case m:MethodSymbol => printMethod(m)
  }

  def printField(f:FieldSymbol): String = f.size match {
    case Some(size) => "field %s: %s[%s]".format(f.id, printDType(f.dtype), size)
    case None => "field %s: %s".format(f.id, printDType(f.dtype))
  }

  def printDType(d:DType): String = d match {
    case DTVoid => "void"
    case DTInt => "int"
    case DTBool => "boolean"
  }

  def printMethod(m:MethodSymbol): String =
    "%s %s(%s) %s".format(printDType(m.returns), m.id, printParams(m.params), printBlock(m.block))

  def printParams(p:SymbolTable): String =
    p.getFields.map(x => "%s %s".format(printDType(x.dtype), x.id)).mkString(", ")

  def printBlock(b:Block): String =
    "{\n%s\n%s\n}".format(indent(lines(b.fields.getFields.map(printField))), 
      indent(lines(b.stmts.map(printStatement))))

  def printPreStmts(preStmts: List[Statement]) = 
    preStmts.map(printStatement(_)).mkString("; ")

  def printStatement(s:Statement): String = s match {
    case Assignment(left, right) => "%s = %s".format(printStore(left), printExpr(right))
    case MethodCall(method, args) => "%s(%s)".format(method.id, printArgs(args))
    case CalloutCall(callout, args) => "%s(%s)".format(callout.id, printArgs(args))
    case If(preStmts, condition, thenb, elseb) => elseb match {
      case Some(block) =>
        "%s if (%s) %s %s".format(printPreStmts(preStmts), printExpr(condition), printBlock(thenb), printBlock(block))
      case None => "if (%s) %s".format(printExpr(condition), printBlock(thenb))
    }
    case For(preStmts, id, start, iter, thenb) =>
      "%s for (%s = %s, %s) %s".format(printPreStmts(preStmts), id, printExpr(start), printExpr(iter), printBlock(thenb))
    case While(preStmts, condition, block, max) => max match {
      case Some(int) =>"while (%s) : %s %s".format(printExpr(condition), int, printBlock(block))
      case None => "%s while (%s) %s %s".format(printPreStmts(preStmts), printExpr(condition), printPreStmts(preStmts), printBlock(block))
    }
    case Return(expr) => expr match {
      case Some(e) => "return %s".format(printExpr(e))
      case None => "return"
    }
    case Break => "break"
    case Continue => "continue"
  }

  def printExpr(expr:Expr): String = expr match {
    case BinOp(left, op, right) => "%s %s %s".format(printExpr(left), op, printExpr(right))
    case UnaryOp(op, right) => "%s%s".format(op, printExpr(right))
    case Ternary(cond, left, right) => "%s ? %s : %s".format(printExpr(cond), printExpr(left), printExpr(right))
    case LoadField(from, index) => index match {
      case Some(i) => "%s[%s]".format(from.id, printExpr(i))
      case None => from.id
    }
    case LoadInt(value) => value.toString
    case LoadBool(value) => value.toString
    case s:Store => printStore(s)
    case MethodCall(method, args) => "%s(%s)".format(method.id, printArgs(args))
    case CalloutCall(callout, args) => "%s(%s)".format(callout.id, printArgs(args))
  
  }

  def printStore(store:Store): String = store.index match {
    case Some(index) => "%s[%s]".format(store.to.id, printExpr(index))
    case None => store.to.id
  }

  def printArgs(args:List[Either[StrLiteral, Expr]]): String = args.map(_ match {
    case Left(str) => "\"%s\"".format(str.value.replace("\n","\\n").replace("\t","\\t"))
    case Right(expr) => printExpr(expr)
  }).mkString(", ")

  // Helpers
  def lines(strs:List[String]): String = if (!strs.nonEmpty) "" else strs.mkString("\n")
  def indent(str:String): String = lines(str.split("\n").map("  " + _).toList)
}
