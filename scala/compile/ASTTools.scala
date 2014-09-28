package compile
import AST._
import org.antlr.runtime.tree.ParseTree

// Construct an AST from a parse tree
object ASTBuilder {
  // Exception for unexpected runtime issues.
  // These represent programming errors and should never happen.
  class ASTConstructionException(msg: String) extends RuntimeException(msg)

  // Wrapper for ParseTree to make the interface a bit nicer.
  class HappyParseTree(pt: ParseTree) {
    def text = pt.getText()
    def children =
      Range(0, pt.getChildCount())
        .map{ i =>
          pt.getChild(i).asInstanceOf[ParseTree]
        } .toList
  }

  implicit def parseTreeUpgrade(pt: ParseTree): HappyParseTree =
    new HappyParseTree(pt)

  // Generic helper to handle sequences of the same node.
  // Takes the container node. For example, call this with
  // a pt pointing to a "field_decls" and node_name of "field_decl"
  // Also skips comma nodes (if comma is not first child!).
  def parseMany[T](pt: ParseTree, node_name: String, processor: ParseTree => T): List[T] =
    pt.children(0).text match {
      case "<epsilon>" => List()
      case `node_name` => pt.children.filter(_.text != ",").map(processor)
    }

  def parseProgram(pt: ParseTree): ProgramAST = ProgramAST(
    parseCalloutDecls(pt.children(0)),
    parseFieldDecls(pt.children(1)),
    parseMethodDecls(pt.children(2)))

  def parseCalloutDecls(pt: ParseTree): List[CalloutDecl] =
    parseMany[CalloutDecl](pt, "callout_decl", parseCalloutDecl)

  def parseCalloutDecl(pt: ParseTree): CalloutDecl =
    CalloutDecl(pt.children(1).text)

  def parseFieldDecls(pt: ParseTree): List[FieldDecl] =
    // This one's a bit funny because there can be multiple
    // FieldDecl's per field_decl tree.
    parseMany[List[FieldDecl]](pt, "field_decl", parseFieldDecl).flatten

  // This returns a List because we unpack comma-sep decls at this stage.
  def parseFieldDecl(pt: ParseTree): List[FieldDecl] = {
    val dtype = parseDType(pt.children(0))
    pt.children.dropRight(1)
      .grouped(2).map(_.tail.head)
      .map(parseFieldDeclRight(_, dtype))
      .toList
  }

  def parseFieldDeclRight(pt: ParseTree, dtype: DType): FieldDecl = {
    val id = pt.children(0).text
    pt.children.length match {
      case 1 => FieldDecl(dtype, id, None)
      case 4 =>
        val size = parseIntLiteral(pt.children(2))
        FieldDecl(dtype, id, Some(size))
    }
  }

  def parseMethodDecls(pt: ParseTree): List[MethodDecl] =
    parseMany[MethodDecl](pt, "method_decl", parseMethodDecl)

  def parseMethodDecl(pt: ParseTree): MethodDecl = {
    val id = pt.children(1).text
    val args = parseMethodDeclArgs(pt.children(3))
    val returns = parseDType(pt.children(0))
    val block = parseBlock(pt.children(5))
    MethodDecl(id, args, returns, block)
  }

  def parseMethodDeclArgs(pt: ParseTree): List[MethodDeclArg] = {
    assert(pt.text == "method_decl_args", pt.text)
    parseMany[MethodDeclArg](pt, "method_decl_arg", parseMethodDeclArg)
  }

  def parseMethodDeclArg(pt: ParseTree): MethodDeclArg = {
    assert(pt.text == "method_decl_arg", pt.text)
    MethodDeclArg(parseDType(pt.children(0)), pt.children(1).text)
  }

  def parseBlock(pt: ParseTree): Block = {
    assert(pt.text == "block")
    val decls = parseFieldDecls(pt.children(1))
    val stmts = parseMany[Statement](pt.children(2), "statement", parseStatement)
    Block(decls, stmts)
  }

  def parseStatement(pt: ParseTree): Statement = {
    assert(pt.text == "statement")
    pt.children(0).text match {
      case "assignment" => parseAssignment(pt.children(0))
      case "method_call" => parseMethodCall(pt.children(0))
      case "if" => throw new ASTConstructionException("not implemented yet")
      case "for" => throw new ASTConstructionException("not implemented yet")
      case "while" => pt.children.length match {
        case 7 => While(
          parseExpr(pt.children(2)),
          parseBlock(pt.children.last), None)
        case 5 => While(
          parseExpr(pt.children(2)),
          parseBlock(pt.children.last),
          Some(parseIntLiteral(pt.children(5))))
      }
      case "return" => pt.children.length match {
        case 3 => Return(Some(parseExpr(pt.children(1))))
        case 2 => Return(None)
      }
      case "break" => Break()
      case "continue" => Continue()
      case x => throw new ASTConstructionException("unrecognized expression type" + x)
    }
  }

  def parseAssignment(pt: ParseTree): Assignment = {
    assert(pt.text == "assignment")
    val left = parseLocation(pt.children(0))
    val right = parseExpr(pt.children(2))
    pt.children(1).children(0).text match {
      case "=" => Assignment(left, right)
      case "+=" => Assignment(left, BinOp(left, "+", right))
      case "-=" => Assignment(left, BinOp(left, "-", right))
    }
  }

  def parseMethodCall(pt: ParseTree): MethodCall = {
    assert(pt.text == "method_call")
    val id = pt.children(0).children(0).text
    val args = parseMethodCallArgs(pt.children(2))
    MethodCall(id, args)
  }

  def parseMethodCallArgs(pt: ParseTree): List[Either[StrLiteral, Expr]] = {
    assert(pt.text == "method_call_args", pt.text)
    parseMany[Either[StrLiteral, Expr]](pt, "method_call_arg", parseMethodCallArg)
  }

  def parseMethodCallArg(pt: ParseTree): Either[StrLiteral, Expr] = {
    assert(pt.text == "method_call_arg")
    pt.children(0).text match {
      case "str_literal" => Left(parseStrLiteral(pt.children(0)))
      case "expr" => Right(parseExpr(pt.children(0)))
    }
  }

  def parseLocation(pt: ParseTree): Location = {
    assert(pt.text == "location")
    pt.children.length match {
      case 4 => Location(pt.children(0).text, Some(parseExpr(pt.children(2))))
      case 1 => Location(pt.children(0).text, None)
    }
  }

  def parseExpr(pt: ParseTree): Expr = {
    // TODO parse exprs.
    BoolLiteral(false)
  }

  def parseDType(pt: ParseTree): DType = pt.text match {
    case "type" => parseDType(pt.children(0))
    case "int" => DTInt
    case "boolean" => DTBool
    case "void" => DTVoid
    case _ => throw new ASTConstructionException("Unknown type " + pt.text)
  }

  def parseIntLiteral(pt: ParseTree): IntLiteral =
    IntLiteral(BigInt(pt.text))

  def parseStrLiteral(pt: ParseTree): StrLiteral = {
    assert(pt.text == "str_literal")
    // TODO this needs to get rid of quotes and undo escapes!
    StrLiteral(pt.children(0).text)
  }

}

object ASTPrinter {
  def printProgram(ast: ProgramAST): String = {
    lines(List(
      lines(ast.callouts.map(printCalloutDecl)),
      lines(ast.fields.map(printFieldDecl)),
      lines(ast.methods.map(printMethodDecl))))
  }

  def printCalloutDecl(ast: CalloutDecl): String = "callout %s".format(ast.id)

  def printFieldDecl(ast: FieldDecl): String = ast match {
    case FieldDecl(dtype, id, None) => "field %s: %s".format(id, printDType(dtype))
    case FieldDecl(dtype, id, Some(size)) =>
      "field %s: %s[%s]".format(id, printDType(dtype), size.value)
  }

  def printMethodDecl(ast: MethodDecl): String = {
    val args = commasep(ast.args .map(arg =>
        "%s: %s".format(arg.id, printDType(arg.dtype))))
    lines(List(
      "%s %s(%s) {".format(
        printDType(ast.returns),
        ast.id,
        args),
      indent(printBlock(ast.block)),
      "}"))
  }

  def printBlock(ast: Block): String = {
    val decls = ast.decls.map(printFieldDecl)
    val stmts = ast.stmts.map(printStatement)
    lines(decls ++ stmts)
  }

  def printStatement(ast: Statement): String = ast match {
    case Assignment(left, right) =>
      "%s = %s".format(printLocation(left), printExpr(right))
    case MethodCall(id, args) => "CANT PRINT MethodCall's YET"
    case If(condition, then, elseb) => "CANT PRINT If's YET"
    case For(id, start, iter, then) => "CANT PRINT If's YET"
    case While(condition, block, max) => "CANT PRINT While's YET"
    case Return(expr) => expr match {
      case Some(expr) => "return " + printExpr(expr)
      case None => "return"
    }
    case Break() => "CANT PRINT Break's YET"
    case Continue() => "CANT PRINT Continue's YET"
  }

  def printLocation(ast: Location): String = ast.index match {
    case Some(index) => "%s[%s]".format(ast.id, index)
    case None => "%s".format(ast.id)
  }

  def printExpr(ast: Expr): String = ast match {
    case MethodCall(id, args) => "(CANT PRINT MethodCall YET)"
    case Location(id, index) => "(CANT PRINT Location YET)"
    case BinOp(left, op, right) => "(CANT PRINT BinOp YET)"
    case UnaryOp(op, right) => "(CANT PRINT UnaryOp YET)"
    case Ternary(condition, left, right) => "(CANT PRINT Ternary YET)"
    case lit: Literal => lit match {
      case IntLiteral(value) => value.toString
      case CharLiteral(value) => "'%s'".format(value)
      case BoolLiteral(value) => value.toString
    }
  }

  def printDType(ast: DType): String = ast match {
    case DTVoid => "void"
    case DTInt => "int"
    case DTBool => "boolean"
  }

  // String formatting helpers
  def lines(strs: List[String]): String = if (!strs.nonEmpty) "" else strs.mkString("\n")
  def commasep(strs: List[String]): String = strs.mkString(", ")
  def indent(str: String): String = lines(str.split("\n").map("  " + _).toList)
}
