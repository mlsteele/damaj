package compile

// Generates unique persistent slugs for objects based on identitiy.
// Note that passing Options into this will likely confuse you.
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
    nameCounter == names.length match {
      case true =>
        counter += 1
        nameCounter = 0
      case false => nameCounter += 1
    }
  }

}
