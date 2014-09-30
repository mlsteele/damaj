package compile;

object SymbolTable {
  import IR._
  import IRShared._

  /**
   * Represents something that can be looked up in a symbol table.
   */
  sealed abstract trait Symbol {
    def id: ID
  }

  case class CalloutSymbol(id: ID) extends Symbol
  case class FieldSymbol(dtype: DType, id: ID, size: Option[IntLiteral]) extends Symbol
  case class MethodSymbol(
    id: ID,
    params: SymbolTable,
    returns: DType,
    block: Block
    ) extends Symbol

  case class Conflict(first: Symbol, second: Symbol)

  type LookupPredicate = Symbol => Boolean

  class SymbolTable (var parent: Option[SymbolTable]) {

    var symbols:List[Symbol] = List();

    def this() = this(None)

    /**
     * Adds a symbol to the table
     * Return value is None if the insert succeeded, and returns a Conflict if the symbol was duplicate.
     * The symbol is considered duplicate if either:
     * - The most local scope contains a field of the same name.
     * - There is a method of the same name in any scope.
     */
    def addSymbol (symbol: Symbol) : Option[Conflict] = {
      // Check there's no local var of same name
      lookupSymbolLocal(symbol.id) match {
        case Some(s) => return Some(Conflict(s, symbol))
        case None => {
          symbols = symbols :+ symbol
          return None;
        }
      }
    }
    
    /**
     * Adds a list of symbols to a table, returns a list of conflicts if there were any.
     */
    def addSymbols (newSymbols: List[Symbol]) : List[Conflict] = {
      var duplicateSymbols: List[Option[Conflict]] = List()
      for (s <- newSymbols) { 
        duplicateSymbols = duplicateSymbols :+ addSymbol(s)
      }
      return duplicateSymbols flatten
    }

    /**
     * Looks up the first field in the current scope that matches the given predicate.
     * For example: table.lookupSymbolLocal(byID("x") and isMethod) tries to find a variable named "x" that is a method.
     */
    def lookupSymbolLocal(id: ID) : Option[Symbol] = {
      for (s <- symbols) {
        if (s.id == id) {
          return Some(s)
        }
      }
      return None
    }

    /**
     * Similar to lookupSymbolLocal, but it also tries looking through the parent scopes.
     */
    def lookupSymbol(id: ID) : Option[Symbol] = {
      lookupSymbolLocal(id) match {
        case Some(s) => Some(s)
        case None    =>  parent.flatMap(_.lookupSymbol(id))
      }
    }
  }
}
