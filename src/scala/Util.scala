package compile

/*
 * Generates unique persistent slugs for objects based on identitiy.
 * Note that passing Option's into this will likely confuse you.
 */
object SlugGenerator {
  private val names = Vector("Martha", "Gerry", "Magdalin", "Hope", "Gertrude", "Roosevelt", "Jim", "Ivan", "Holden", "Winter", "Sandra", "Cassidy", "Robert", "Uneek", "Christ", "Albert")
  private var nameCounter = 0
  private var counter = 0
  private val ids = new IdentityMap[Any, String]()

  def id(x: Any) = {
    ids(x) match {
      case Some(id) => id
      case None =>
        val name = names(nameCounter)
        val id = s"$name$counter"
        advance()
        ids.put(x, id)
        id
    }
  }

  private def advance() = {
    (nameCounter + 1) == names.length match {
      case true =>
        counter += 1
        nameCounter = 0
      case false => nameCounter += 1
    }
  }

}

/*
 * Hack to support reference equality mappings for any object.
 * Uses native java collection internally.
 */
// TODO mutable?
class IdentityMap[K,V] {
  import java.util.IdentityHashMap
  import collection.JavaConversions._

  private val store = new IdentityHashMap[K,V]

  def get(k: K): Option[V] = Option(store.get(k))

  def apply(k: K): Option[V] = get(k)

  def put(k: K, v: V): Unit = {
    store.put(k, v)
    return ()
  }

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

/**
  * Utilities for analyzing what variables a expression depends on.
  */
object ExprDependencies {
  import IRShared._
  import IR2._

  implicit class BetterExpr(e: Expr) {
    // Set of loads that this expression depends on
    // We assume that if an expr accesses any element of an array, it depends on the entire array
    def dependencies(): Set[Load] = e match {
      case Call(_, args) =>
        args.toSet.flatMap{ x: Either[StrLiteral, Expr] => x match {
          case Left(str) => List()
          case Right(subE) => subE.dependencies().toList
        }}
      case BinOp(left, _, right) => Set(left, right)
      case UnaryOp(_, right) => Set(right)
      case l@LoadField(to, index) =>
        Set(LoadField(to, None)) ++ index.map(_.dependencies()).getOrElse(Set())
      case _:LoadLiteral => Set()
    }
  }
}
