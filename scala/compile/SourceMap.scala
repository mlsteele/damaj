package compile

case class SourceMapEntry(line: Int, char: Int)

class SourceMap() {
  import org.antlr.runtime.tree.ParseTree
  import org.antlr.runtime.Token

  type Key = Any

  private var store: Map[Any, SourceMapEntry] = Map()

  // Failing a lookup in a SourceMap is a programming error.
  class SourceMapBadLookup(msg: String) extends RuntimeException(msg)

  def apply(key: Key): SourceMapEntry = store.get(key) match {
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
}
