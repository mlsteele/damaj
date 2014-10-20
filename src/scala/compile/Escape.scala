package compile

object Escape {
  import scala.util.matching.Regex.Match

  def unescape(s: String): String = "(\\\\.)".r.replaceAllIn(s, (m: Match) => unescapeSuspicious(m.matched))

  // x must start with a backslash
  def unescapeSuspicious(x: String): String = {
    assert(x.length == 2)
    x match {
      // WHAT!??
      case "\\\\" => "\\\\"
      case "\\\"" => "\""
      case "\\'" => "'"
      case "\\n" => "\n"
      case "\\t" => "\t"
    }
  }

  def escape(x: Char): String = x match {
    case '\\' => "\\\\"
    case '\"' => "\\\""
    case ''' => "\\'"
    case '\n' => "\\n"
    case '\t' => "\\t"
    case _ => x.toString
  }

  def escape(x: String): String = x.flatMap((c: Char) => escape(c))

  def tests: Boolean = {
    true
  }
}
