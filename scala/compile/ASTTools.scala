package compile
import org.antlr.runtime.tree.ParseTree

// Construct an AST from a parse tree
// ptree - root program node
// source - source code
// Example Usage:
//   val ast = ASTBuilder.parseProgram(parseTree).ast
class ASTBuilder(ptree: ParseTree, filepath: String, code: String) {
  import IRShared._
  import AST._
  import PTTools.HappyParseTree

  // Every time an AST node is constructed, it must also be added to srcmap.
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
    pt.children(1))

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
    val node = pt.children.length match {
      case 1 => FieldDecl(dtype, id, None)
      case 4 =>
        val size = parseIntLiteral(pt.children(2))
        FieldDecl(dtype, id, Some(size))
    }
    srcmap.add(node, pt.children(0))
  }

  def parseMethodDecls(pt: ParseTree): List[MethodDecl] =
    parseMany[MethodDecl](pt, "method_decl", parseMethodDecl)

  def parseMethodDecl(pt: ParseTree): MethodDecl = {
    val id = pt.children(1).text
    val args = parseMethodDeclArgs(pt.children(3))
    val returns = parseDType(pt.children(0))
    val block = parseBlock(pt.children(5))
    srcmap.add(
      MethodDecl(id, args, returns, block),
      pt.children(1))
  }

  def parseMethodDeclArgs(pt: ParseTree): List[MethodDeclArg] = {
    assert(pt.text == "method_decl_args", pt.text)
    parseMany[MethodDeclArg](pt, "method_decl_arg", parseMethodDeclArg)
  }

  def parseMethodDeclArg(pt: ParseTree): MethodDeclArg = {
    assert(pt.text == "method_decl_arg", pt.text)
    srcmap.add(
      MethodDeclArg(parseDType(pt.children(0)), pt.children(1).text),
      pt.children(1))
  }

  def parseBlock(pt: ParseTree): Block = {
    assert(pt.text == "block")
    val decls = parseFieldDecls(pt.children(1))
    val stmts = parseMany[Statement](pt.children(2), "statement", parseStatement)
    srcmap.add(
      Block(decls, stmts),
      pt.children(0))
  }

  def parseStatement(pt: ParseTree): Statement = {
    assert(pt.text == "statement")
    val head: ParseTree = pt.children(0)
    head.text match {
      case "assignment" => parseAssignment(head)
      case "method_call" => parseMethodCall(head)
      case "if" => pt.children.length match {
        case 5 =>
          val expr = parseExpr(pt.children(2))
          val then = parseBlock(pt.children(4))
          srcmap.add(
            If(expr, then, None),
            head)
        case 7 =>
          val expr = parseExpr(pt.children(2))
          val then = parseBlock(pt.children(4))
          val elseb = Some(parseBlock(pt.children(6)))
          srcmap.add(
            If(expr, then, elseb),
            head)
      }
      case "for" => srcmap.add(
        For(pt.children(2).text,
            parseExpr(pt.children(4)),
            parseExpr(pt.children(6)),
            parseBlock(pt.children(8))),
        head)
      case "while" => pt.children.length match {
        case 5 => srcmap.add(
          While(
            parseExpr(pt.children(2)),
            parseBlock(pt.children.last), None),
          head)
        case 7 => srcmap.add(
          While(
            parseExpr(pt.children(2)),
            parseBlock(pt.children.last),
            Some(parseIntLiteral(pt.children(5)))),
          head)
      }
      case "return" => pt.children.length match {
        case 3 => srcmap.add(
          Return(Some(parseExpr(pt.children(1)))),
          head)
        case 2 => srcmap.add(Return(None), head)
      }
      case "break" => srcmap.add(Break, head)
      case "continue" => srcmap.add(Continue, head)
      case x => throw new ASTConstructionException("unrecognized expression type" + x)
    }
  }

  def parseAssignment(pt: ParseTree): Assignment = {
    assert(pt.text == "assignment")
    val left = parseLocation(pt.children(0))
    val right = parseExpr(pt.children(2))
    val op = pt.children(1).children(0).text
    val node = op match {
      case "=" => Assignment(left, right)
      case "+=" | "-=" =>
        // sketchy string operation op(0) extracts the + or -
        assert(List("-", "+").contains(op(0).toString))
        val implied = BinOp(left, op(0).toString, right)
        srcmap.add(implied, pt.children(1).children(0))
        Assignment(left, implied)
    }
    srcmap.add(node, pt.children(1).children(0))
  }

  def parseMethodCall(pt: ParseTree): MethodCall = {
    assert(pt.text == "method_call")
    val id = pt.children(0).children(0).text
    val args = parseMethodCallArgs(pt.children(2))
    srcmap.add(
      MethodCall(id, args),
      pt.children(0).children(0))
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
    val node = pt.children.length match {
      case 4 => Location(pt.children(0).text, Some(parseExpr(pt.children(2))))
      case 1 => Location(pt.children(0).text, None)
    }
    srcmap.add(node, pt.children(0))
  }

  def parseExpr(pt: ParseTree): Expr = {
    assert(pt.text == "expr", pt.toStringTree)
    parseExprInner(pt.children(0))
  }

  def parseExprInner(pt: ParseTree): Expr = {
    val length = pt.children.length
    val c = pt.children
    (pt.text, length) match {
      // base case
      case ("eJ", _) => parseExprInnerDeepest(pt)
      // descend
      case (_, 1) => parseExprInner(c(0))
      case _ => pt.text match {
        // ternary
        case "eB" =>
          val condition = parseExprInner(c(0))
          val left = parseExprInner(c(2))
          val right = parseExprInner(c(4))
          srcmap.add(Ternary(condition, left, right), c(1))
        // binary ops
        case "eC" | "eD" | "eE" | "eF" | "eG" | "eH" =>
          processLeftToRight(c)
        // unary ops- ! @
        case "eI" =>
          srcmap.add(UnaryOp(c(0).text, parseExprInner(c(1))), c(0))
        // parens
        case "eZ" => parseExprInner(c(1))
      }
    }
  }

  // Consume binops in a left assosciative manner.
  // list must be of length 1+n*2
  def processLeftToRight(list: List[ParseTree]): Expr = list match {
    case List(operand) => parseExprInner(operand)
    case List(left, op, right) =>
      parseBinOp(parseExprInner(left), op, parseExprInner(right))
    case _ =>
      val lefts = list.dropRight(2)
      val op    = list.takeRight(2)(0)
      val right = list.takeRight(1)(0)
      parseBinOp(processLeftToRight(lefts), op, parseExprInner(right))
  }

  def parseBinOp(left: Expr, center: ParseTree, right: Expr): BinOp =
    srcmap.add(BinOp(left, center.text, right), center)

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
    case "int" => srcmap.add(DTInt, pt)
    case "boolean" => srcmap.add(DTBool, pt)
    case "void" => srcmap.add(DTVoid, pt)
    case _ => throw new ASTConstructionException("Unknown type " + pt.text)
  }

  def parseLiteral(pt: ParseTree): Literal = {
    assert(pt.text == "literal")
    val child = pt.children(0)
    val gchild = child.children(0)
    val node = child.text match {
      case "int_literal" => parseIntLiteral(gchild)
      case "bool_literal" => gchild.text match {
        case "true" => srcmap.add(BoolLiteral(true), gchild)
        case "false" => srcmap.add(BoolLiteral(false), gchild)
      }
      case "char_literal" => parseCharLiteral(child)
    }
    val wrapped = Literal(node)
    srcmap.add(wrapped, gchild)
  }

  def parseIntLiteral(pt: ParseTree): IntLiteral = srcmap.add(
    IntLiteral(BigInt(pt.text)),
    pt)

  def parseStrLiteral(pt: ParseTree): StrLiteral = {
    assert(pt.text == "str_literal")
    val inner = pt.children(0).text.drop(1).dropRight(1)
    srcmap.add(
      StrLiteral(Escape.unescape(inner)),
      pt.children(0))
  }

  def parseCharLiteral(pt: ParseTree): CharLiteral = {
    assert(pt.text == "char_literal")
    val inner = pt.children(0).text.drop(1).dropRight(1)
    val str = Escape.unescape(inner)
    assert(str.length == 1, str)
    srcmap.add(
      CharLiteral(str(0)),
      pt.children(0))
  }

}

// Example Usage:
//   println(new ASTPrinter(ast).print)
class ASTPrinter(ast: AST.ProgramAST) {
  import IRShared._
  import AST._

  val srcmap = ast.srcmap
  val print = printASTNode(ast)

  def annotate(ast: ASTNode, rep: String): String = {
    val replines: List[String] = rep.split("\n").toList
    val e: SourceMapEntry = srcmap(ast)
    lines(
      "%s (%s:%s)".format(replines(0), e.line, e.char)
      +: replines.drop(1))
  }

  def printASTNode(ast: ASTNode): String = {
    ast match {
      case ast: ProgramAST => printProgram(ast)
      case ast: CalloutDecl => annotate(ast, printCalloutDecl(ast))
      case ast: FieldDecl => annotate(ast, printFieldDecl(ast))
      case ast: MethodDecl => annotate(ast, printMethodDecl(ast))
      case ast: MethodDeclArg => printMethodDeclArg(ast)
      case ast: Block => printBlock(ast)
      case ast: MethodCall => printMethodCall(ast)
      case ast: Statement => annotate(ast, printStatement(ast))
      case ast: Location => printLocation(ast)
      case ast: Expr => printExpr(ast)
      case ast: DType => printDType(ast)
    }
  }

  def printProgram(ast: ProgramAST): String = {
    lines(List(
      lines(ast.callouts.map(printASTNode)),
      lines(ast.fields.map(printASTNode)),
      lines(ast.methods.map(printASTNode))))
  }

  def printCalloutDecl(ast: CalloutDecl): String =
    "callout %s".format(ast.id)

  def printFieldDecl(ast: FieldDecl): String = ast match {
    case FieldDecl(dtype, id, None) => "field %s: %s".format(id, printDType(dtype))
    case FieldDecl(dtype, id, Some(size)) =>
      "field %s: %s[%s]".format(id, printDType(dtype), size.value)
  }

  def printMethodDecl(ast: MethodDecl): String = {
    val args = commasep(ast.args.map(printASTNode))
    lines(List(
      "%s %s(%s) {".format(
        printDType(ast.returns),
        ast.id,
        args),
      indent(printASTNode(ast.block)),
      "}"))
  }

  def printMethodDeclArg(ast: MethodDeclArg): String =
    "%s: %s".format(ast.id, printDType(ast.dtype))

  def printBlock(ast: Block): String = {
    val decls = ast.decls.map(printASTNode)
    val stmts = ast.stmts.map(printASTNode)
    lines(decls ++ stmts)
  }

  def printStatement(ast: Statement): String = ast match {
    case Assignment(left, right) =>
      "%s = %s".format(printASTNode(left), printASTNode(right))
    case ast: MethodCall => printASTNode(ast)
    case If(condition, then, elseb) => elseb match {
      case Some(elseb) => "if (%s) {\n%s\n} else {\n%s\n}".format(
        printASTNode(condition),
        indent(printASTNode(then)),
        indent(printASTNode(elseb)))
      case None => "if (%s) {\n%s\n}".format(
        printASTNode(condition),
        indent(printASTNode(then)))
    }
    case For(id, start, iter, then) => "for (%s = %s, %s) {\n%s\n}".format(
      id, printASTNode(start), printASTNode(iter), indent(printASTNode(then)))
    case While(condition, block, max) => max match {
      case None => "while (%s) {\n%s\n}".format(
        printASTNode(condition), printASTNode(block))
      case Some(max) => "while (%s): %s {\n%s\n}".format(
        printASTNode(condition), max.value, printASTNode(block))
    }
    case Return(expr) => expr match {
      case Some(expr) => "return " + printASTNode(expr)
      case None => "return"
    }
    case Break => "break"
    case Continue => "continue"
  }

  def printExpr(ast: Expr): String = ast match {
    case ast: MethodCall => printASTNode(ast)
    case ast: Location => printASTNode(ast)
    case BinOp(left, op, right) => "(%s %s %s)".format(printASTNode(left), op, printExpr(right))
    case UnaryOp(op, right) => "%s%s".format(op, printASTNode(right))
    case Ternary(cond, left, right) =>
      "%s ? %s : %s".format(printASTNode(cond), printASTNode(left), printASTNode(right))
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
        case Right(expr) => printASTNode(expr)
      }
    }.mkString(", "))
  }

  def printLocation(ast: Location): String = ast.index match {
    case Some(index) => "%s[%s]".format(ast.id, printASTNode(index))
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
