package compile

case class SourceMapEntry(line: Int, char: Int)

// code - full text of the program to compile
class SourceMap(filepath: String, code: String) {
  import org.antlr.runtime.tree.ParseTree
  import org.antlr.runtime.Token

  type Key = Any

  private var store: Map[Any, SourceMapEntry] = Map()
  private val codelines = code.split("\n")

  // Failing a lookup in a SourceMap is a programming error.
  class SourceMapBadLookup(msg: String) extends RuntimeException(msg)

  // Conveniently lookup with srcmap(node)
  def apply(key: Key): SourceMapEntry = get(key)

  def get(key: Key): SourceMapEntry = store.get(key) match {
    case Some(x) => x
    case None => throw new SourceMapBadLookup("Can't find '%s' in this source map".format(key))
  }

  def add[T](key: T, e: SourceMapEntry): T = {
    store += key -> e
    return key
  }

  // pt must be a leaf node (whose payload is a Token, not a String)
  def add[T](key: T, pt: ParseTree): T = {
    val pl = pt.payload.asInstanceOf[Token]
    val entry = SourceMapEntry(pl.getLine(), pl.getCharPositionInLine())
    add(key, entry)
  }

  // Format an error message.
  // Example:
  // foo/bar/baz.dcf:12: Undefined symbol 'y'.
  // x = a + b + y + z;
  //             ^
  def report(problemNode: Key, msg: String): String = {
    val e: SourceMapEntry = get(problemNode)
    lines(List(
      "%s:%s: %s".format(filepath, e.line, msg),
      codelines(e.line-1),
      "%s^".format(" " * e.char)))
  }

  def lines(strs: List[String]): String = if (!strs.nonEmpty) "" else strs.mkString("\n")
}
