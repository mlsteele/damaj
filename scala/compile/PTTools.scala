package compile

// Tools for manipulating ANTLR ParseTree's
object PTTools {
  import org.antlr.runtime.tree.ParseTree
  import org.antlr.runtime.Token

  // Wrapper for ParseTree to make the interface a bit nicer.
  class HappyParseTree(pt: ParseTree) {
    def text = pt.getText()
    def children =
      Range(0, pt.getChildCount())
        .map{ i =>
          pt.getChild(i).asInstanceOf[ParseTree]
        } .toList
  }

  def prettyprint(tree: ParseTree): String = {
    val children = Range(0, tree.getChildCount()).map (tree.getChild(_).asInstanceOf[ParseTree])
    val text = tree.getText()
    val prettyline = tree.payload match {
      case pl: String =>
        // payload is just the same as text
        "%s".format(text)
      case pl: Token =>
        val srcline = pl.getLine()
        val srcchar = pl.getCharPositionInLine()
        val ttype = pl.getType()
        "%s (t:%s, l:%s:%s)".format(text, ttype, srcline, srcchar)
    }

    children.length match {
      case 0 => prettyline
      case _ =>
        val childstrs = indent(lines(children.map(prettyprint).toList))
        lines(List(prettyline, childstrs))
    }
  }

  def lines(strs: List[String]): String = if (!strs.nonEmpty) "" else strs.mkString("\n")
  def indent(str: String): String = lines(str.split("\n").map("  " + _).toList)
}
