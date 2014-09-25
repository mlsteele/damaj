package compile
import compile.AST._
import org.antlr.runtime.tree.ParseTree

// Tools for handling ASTs.
object ASTTools {
  class ASTConstructionException() extends RuntimeException

  // Get the children of a ParseTree node.
  def pNodeChildren(tree: ParseTree): List[ParseTree] =
    Range(0, tree.getChildCount())
      .map(tree.getChild(_).asInstanceOf[ParseTree])
      .toList

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
