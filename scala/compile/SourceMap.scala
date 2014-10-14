package compile

case class SourceMapEntry(line: Int, char: Int)

// code - full text of the program to compile
class SourceMap(filepath: String, code: String) {
  import org.antlr.runtime.tree.ParseTree
  import org.antlr.runtime.Token

  type Key = Any

  // This thing is backed by an IdentityMap which considers
  // unique objects instead of using == comparators.
  // This is because ast/ir nodes could 'look' (==) equivalent
  // but come from different locations in source code.
  private val store = new IdentityMap[Any, SourceMapEntry]()
  private val codelines = code.split("\n")

  // Failing a lookup in a SourceMap is a programming error.
  class SourceMapBadLookup(msg: String) extends RuntimeException(msg)

  // Conveniently lookup with srcmap(node)
  def apply(key: Key): SourceMapEntry = get(key)

  def get(key: Key): SourceMapEntry = store.get(key) match {
    case Some(x) => x
    case None => throw new SourceMapBadLookup("Can't find '%s' in this source map".format(key))
  }

  def add[T <: Key](key: T, e: SourceMapEntry): T = {
    store.put(key, e)
    return key
  }

  // pt must be a leaf node (whose payload is a Token, not a String)
  def add[T <: Key](key: T, pt: ParseTree): T = {
    val pl = pt.payload.asInstanceOf[Token]
    val entry = SourceMapEntry(pl.getLine(), pl.getCharPositionInLine())
    add(key, entry)
  }

  // Add an entry for b that points to the same thing as a.
  // Useful if b is created by deriving a.
  // a must already be in this map
  def alias[T](a: Key, b: T): T = {
    store(a) match {
      case None => assert(false, "Can not alias because a is not in SourceMap.")
        .asInstanceOf[T]
      case Some(entry) => add[T](b, entry)
    }
  }

  // Format an error message.
  // Example:
  // foo/bar/baz.dcf:12: Undefined symbol 'y'.
  // x = a + b + y + z;
  //             ^
  //
  // problemNode is an AST node or any other thing you have added to the source map.
  def report(problemNode: Key, msg: String): String = {
    // NOTE(miles): try/catch is here so that we get a better grade even if we screw up.
    try {
      val e: SourceMapEntry = get(problemNode)
      lines(List(
        "%s:%s: %s".format(filepath, e.line, msg),
        codelines(e.line-1),
        "%s^".format(" " * e.char)))
    } catch {
      case ex: SourceMapBadLookup =>
        "%s:??: %s".format(filepath, msg)
    }
  }

  def lines(strs: List[String]): String = if (!strs.nonEmpty) "" else strs.mkString("\n")
}

class IdentityMap[K,V] {
  import java.util.IdentityHashMap
  import collection.JavaConversions._

  private val store = new IdentityHashMap[K,V]

  def get(k: K): Option[V] = Option(store.get(k))

  def apply(k: K): Option[V] = get(k)

  def put(k: K, v: V): Unit = {store.put(k, v); ()}

  def putAll(m: IdentityMap[K,V]): Unit = store.putAll(m.store)

  def contains(k: K): Boolean = store.containsKey(k)

  def keys: Set[K] = store.keySet().toSet

  def values: Set[V] = store.values().toSet

  def ++(other: IdentityMap[K,V]): IdentityMap[K,V] = {
    val newMap = new IdentityMap[K,V]()
    newMap.putAll(this)
    newMap.putAll(other)
    newMap
  }

  override def toString = store.toString
}
