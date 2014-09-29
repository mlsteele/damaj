package compile
import org.antlr.runtime.tree.ParseTree

// Construct an AST from a parse tree
// ptree - root program node
// source - source code
// Example Usage:
//   val ast = ASTBuilder.parseProgram(parseTree).ast
class ASTBuilder(ptree: ParseTree, filepath: String, code: String) {
// class ASTBuilder(ptree: ParseTree, source: String) {
  import IRShared._
  import AST._
  import PTTools.HappyParseTree

  val srcmap = new SourceMap(filepath, code)
  val ast = parseProgram(ptree)

  // Exception for unexpected runtime issues.
  // These represent programming errors and should never happen.
  class ASTConstructionException(msg: String) extends RuntimeException(msg)

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

  def parseProgram(pt: ParseTree): ProgramAST = srcmap.add(
    ProgramAST(
      parseCalloutDecls(pt.children(0)),
      parseFieldDecls(pt.children(1)),
      parseMethodDecls(pt.children(2)),
      srcmap),
    SourceMapEntry(0, 0))

  def parseCalloutDecls(pt: ParseTree): List[CalloutDecl] =
    parseMany[CalloutDecl](pt, "callout_decl", parseCalloutDecl)

  def parseCalloutDecl(pt: ParseTree): CalloutDecl = srcmap.add(
    CalloutDecl(pt.children(1).text),
    pt.children(0))

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
      case "if" => pt.children.length match {
        case 5 => If(parseExpr(pt.children(2)), parseBlock(pt.children(4)), None)
        case 7 =>
          If(parseExpr(pt.children(2)), parseBlock(pt.children(4)), Some(parseBlock(pt.children(6))))
      }
      case "for" => For(pt.children(2).text,
                        parseExpr(pt.children(4)),
                        parseExpr(pt.children(6)),
                        parseBlock(pt.children(8)))
      case "while" => pt.children.length match {
        case 5 => While(
          parseExpr(pt.children(2)),
          parseBlock(pt.children.last), None)
        case 7 => While(
          parseExpr(pt.children(2)),
          parseBlock(pt.children.last),
          Some(parseIntLiteral(pt.children(5))))
      }
      case "return" => pt.children.length match {
        case 3 => Return(Some(parseExpr(pt.children(1))))
        case 2 => Return(None)
      }
      case "break" => Break
      case "continue" => Continue
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
    assert(pt.text == "expr", pt.toStringTree)
    parseExprInner(pt.children(0))
  }

  def parseExprInner(pt: ParseTree): Expr = {
    val length = pt.children.length
    val c = pt.children
    (pt.text, length) match {
      case ("eJ", _) => parseExprInnerDeepest(pt)
      case (_, 1) => parseExprInner(c(0))
      case _ => pt.text match {
        // ternary
        case "eB" =>
          val condition = parseExprInner(c(0))
          val left = parseExprInner(c(2))
          val right = parseExprInner(c(4))
          Ternary(condition, left, right)
        // binary ops
        case "eC" | "eD" | "eE" | "eF" | "eG" | "eH" =>
          val left = parseExprInner(c(0))
          val right = parseExprInner(c(2))
          BinOp(left, c(1).text, right)
        // unary ops- ! @
        case "eI" =>
          UnaryOp(c(0).text, parseExprInner(c(1)))
      }
    }
  }

  def parseExprInnerDeepest(pt: ParseTree): Expr = {
    assert(pt.text == "eJ")
    val child = pt.children(0)
    child.text match {
      case "location" => parseLocation(child)
      case "method_call" => parseMethodCall(child)
      case "literal" => parseLiteral(child)
    }
  }

  def parseDType(pt: ParseTree): DType = pt.text match {
    case "type" => parseDType(pt.children(0))
    case "int" => DTInt
    case "boolean" => DTBool
    case "void" => DTVoid
    case _ => throw new ASTConstructionException("Unknown type " + pt.text)
  }

  def parseLiteral(pt: ParseTree): Literal = {
    assert(pt.text == "literal")
    val child = pt.children(0)
    child.text match {
      case "int_literal" => Literal(parseIntLiteral(child.children(0)))
      case "bool_literal" => child.children(0).text match {
        case "true" => Literal(BoolLiteral(true))
        case "false" => Literal(BoolLiteral(false))
      }
      case "char_literal" => Literal(parseCharLiteral(child))
    }
  }

  def parseIntLiteral(pt: ParseTree): IntLiteral = IntLiteral(BigInt(pt.text))

  def parseStrLiteral(pt: ParseTree): StrLiteral = {
    assert(pt.text == "str_literal")
    val inner = pt.children(0).text.drop(1).dropRight(1)
    StrLiteral(Escape.unescape(inner))
  }

  def parseCharLiteral(pt: ParseTree): CharLiteral = {
    assert(pt.text == "char_literal")
    val inner = pt.children(0).text.drop(1).dropRight(1)
    val str = Escape.unescape(inner)
    assert(str.length == 1, str)
    CharLiteral(str(0))
  }

}

// Example Usage:
//   println(new ASTPrinter(ast).print)
class ASTPrinter(ast: AST.ProgramAST) {
  import IRShared._
  import AST._

  val srcmap = ast.srcmap
  val print = printProgram(ast)

  def printProgram(ast: ProgramAST): String = {
    lines(List(
      lines(ast.callouts.map(printCalloutDecl)),
      lines(ast.fields.map(printFieldDecl)),
      lines(ast.methods.map(printMethodDecl))))
  }

  def src(ast: srcmap.Key): String = {
    val e: SourceMapEntry = srcmap(ast)
    " (%s:%s)".format(e.line, e.char)
  }

  def printCalloutDecl(ast: CalloutDecl): String =
    "callout %s%s".format(ast.id, src(ast))

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
    case ast: MethodCall => printMethodCall(ast)
    case If(condition, then, elseb) => elseb match {
      case Some(elseb) => "if (%s) {\n%s\n} else {\n%s\n}".format(
        printExpr(condition),
        indent(printBlock(then)),
        indent(printBlock(elseb)))
      case None => "if (%s) {\n%s\n}".format(
        printExpr(condition),
        indent(printBlock(then)))
    }
    case For(id, start, iter, then) => "for (%s = %s, %s) {\n%s\n}".format(
      id, printExpr(start), printExpr(iter), indent(printBlock(then)))
    case While(condition, block, max) => max match {
      case None => "while (%s) {\n%s\n}".format(
        printExpr(condition), printBlock(block))
      case Some(max) => "while (%s): %s {\n%s\n}".format(
        printExpr(condition), max.value, printBlock(block))
    }
    case Return(expr) => expr match {
      case Some(expr) => "return " + printExpr(expr)
      case None => "return"
    }
    case Break => "break"
    case Continue => "continue"
  }

  def printExpr(ast: Expr): String = ast match {
    case ast: MethodCall => printMethodCall(ast)
    case ast: Location => printLocation(ast)
    case BinOp(left, op, right) => "(%s %s %s)".format(printExpr(left), op, printExpr(right))
    case UnaryOp(op, right) => "%s%s".format(op, printExpr(right))
    case Ternary(cond, left, right) =>
      "%s ? %s : %s".format(printExpr(cond), printExpr(left), printExpr(right))
    case lit: Literal => lit.inner match {
      case IntLiteral(value) => value.toString
      case CharLiteral(value) => "'%s'".format(Escape.escape(value))
      case BoolLiteral(value) => value.toString
    }
  }

  def printMethodCall(ast: MethodCall): String = {
    "%s(%s)".format(ast.id, ast.args.map{ arg =>
      arg match {
        case Left(strl) => "\"%s\"".format(Escape.escape(strl.value))
        case Right(expr) => printExpr(expr)
      }
    }.mkString(", "))
  }

  def printLocation(ast: Location): String = ast.index match {
    case Some(index) => "%s[%s]".format(ast.id, printExpr(index))
    case None => "%s".format(ast.id)
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
