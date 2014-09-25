package compile
import compile.AST._
import org.antlr.runtime.tree.ParseTree

// Tools for handling ASTs.
object ASTTools {
  class ASTConstructionException(msg: String) extends RuntimeException(msg)

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

  def parseProgram(pt: ParseTree): ProgramAST = ProgramAST(
    parseCalloutDecls(pt.children(0)),
    parseFieldDecls(pt.children(1)),
    parseMethodDecls(pt.children(2)))

  def parseCalloutDecls(pt: ParseTree): List[CalloutDecl] =
    pt.children(0).text match {
      case "callout_decl" => pt.children.map(parseCalloutDecl(_)).toList
      case "<epsilon>" => List()
      case _ => throw new ASTConstructionException("wtf")
    }

  def parseCalloutDecl(pt: ParseTree): CalloutDecl =
    CalloutDecl(pt.children(1).text)

  def parseFieldDecls(pt: ParseTree): List[FieldDecl] =
    pt.children(0).text match {
      case "field_decl" => pt.children.map(parseFieldDecl(_)).toList
      case "<epsilon>" => List()
      case _ => throw new ASTConstructionException("wtf")
    }

  def parseFieldDecl(pt: ParseTree): FieldDecl = {
    // TODO only handles simplest case fields.
    val dtype = parseDType(pt.children(0))
    val id = pt.children(1).children(0).text
    FieldDecl(dtype, id, None)
  }

  def parseDType(pt: ParseTree): DType = pt.text match {
    case "type" => parseDType(pt.children(0))
    case "int" => DTInt
    case "boolean" => DTBool
    case "void" => DTVoid
    case _ => throw new ASTConstructionException("Unknown type " + pt.text)
  }

  def parseMethodDecls(pt: ParseTree): List[MethodDecl] =
    pt.children(0).text match {
      case "method_decl" => pt.children.map(parseMethodDecl(_)).toList
      case "<epsilon>" => List()
      case _ => throw new ASTConstructionException("wtf")
    }

  def parseMethodDecl(pt: ParseTree): MethodDecl = {
    // TODO args and block are fake.
    val id = pt.children(1).text
    val args = List()
    val returns = parseDType(pt.children(0))
    val block = Block(List(), List())
    MethodDecl(id, args, returns, block)
  }

}
