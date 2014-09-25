package compile
import compile.AST._
import org.antlr.runtime.tree.ParseTree

// Tools for handling ASTs.
object ASTTools {
  class ASTConstructionException() extends RuntimeException

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

  // Get the children of a ParseTree node.
  def pNodeChildren(tree: ParseTree): List[ParseTree] =
    Range(0, tree.getChildCount())
      .map(tree.getChild(_).asInstanceOf[ParseTree])
      .toList

  def parseProgram(pt: ParseTree): ProgramAST = ProgramAST(
    parseCalloutDecls(pt.children(0)),
    parseFieldDecls(pt.children(1)),
    parseMethodDecls(pt.children(2)))

  def parseCalloutDecls(pt: ParseTree): List[CalloutDecl] =
    pt.children(0).text match {
      case "callout_decl" => pt.children.map(parseCalloutDecl(_)).toList
      case "<epsilon>" => List()
      case _ => throw new ASTConstructionException
    }

  def parseCalloutDecl(pt: ParseTree): CalloutDecl =
    CalloutDecl(pt.children(1).text)

  def parseFieldDecls(pt: ParseTree): List[FieldDecl] =
    pt.children(0).text match {
      case "field_decl" => pt.children.map(parseFieldDecl(_)).toList
      case "<epsilon>" => List()
      case _ => throw new ASTConstructionException
    }

  def parseFieldDecl(pt: ParseTree): FieldDecl = {
    // TODO only handles simplest case fields.
    val dtype = parseDType(pt.children(0).children(0).text)
    val id = pt.children(1).children(0).text
    FieldDecl(dtype, id, None)
  }

  def parseDType(s: String): DType = s match {
    case "int" => DTInt
    case "boolean" => DTBool
    case "void" => DTVoid
    case _ => throw new ASTConstructionException
  }


  def parseMethodDecls(pt: ParseTree): List[MethodDecl] =
    pt.children(0).text match {
      case "method_decl" => pt.children.map(parseMethodDecl(_)).toList
      case "<epsilon>" => List()
      case _ => throw new ASTConstructionException
    }

  def parseMethodDecl(pt: ParseTree): MethodDecl = {
    // TODO args and block are fake.
    val id = pt.children(1).text
    val args = List()
    val returns = parseDType(pt.children(0).text)
    val block = Block(List(), List())
    MethodDecl(id, args, returns, block)
  }

  // Parse a ParseTree into an AST node.
  // The AST type returned depends on the ParseTree input.
  def parseNode(pt: ParseTree): Any = {
    val kind = pt.getText()
    val children = pNodeChildren(pt)
    kind match {
      case "program" => {
        // TODO don't do it.
        val callouts = List()
        val fields = List(parseNode(children(0)).asInstanceOf[FieldDecl])
        val methods = List()
        ProgramAST(callouts, fields, methods)
      }
      case "field_decl" => {
        val dtype: DType = parseNode(children(0)).asInstanceOf[DType]
        // TODO only works for single simple decls.
        val id = pNodeChildren(children(1))(0).getText()
        FieldDecl(dtype, id, None)
      }
      case "type" => children(0).getText() match {
        case "int" => DTInt
        case "boolean" => DTBool
      }
      case _ => throw new ASTConstructionException
    }
  }

}
