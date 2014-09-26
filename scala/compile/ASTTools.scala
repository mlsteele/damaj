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
      case _ => throw new ASTConstructionException("wtf")
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
    // TODO block
    val id = pt.children(1).text
    val args = parseMethodDeclArgs(pt.children(3))
    val returns = parseDType(pt.children(0))
    val block = Block(List(), List())
    MethodDecl(id, args, returns, block)
  }

  def parseMethodDeclArgs(pt: ParseTree): List[MethodDeclArg] = {
    assert(pt.text == "method_decl_args")
    parseMany[MethodDeclArg](pt, "method_decl_arg", parseMethodDeclArg)
  }

  def parseMethodDeclArg(pt: ParseTree): MethodDeclArg = {
    assert(pt.text == "method_decl_arg", pt.text)
    MethodDeclArg(parseDType(pt.children(0)), pt.children(1).text)
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

  def printStatement(ast: Statement): String =
    // TODO
    "CAN'T PRINT STATEMENTS YET"

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
