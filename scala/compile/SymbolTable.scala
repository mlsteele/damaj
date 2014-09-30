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

  type LookupPredicate = Symbol => Boolean

  class SymbolTable (var parent: Option[SymbolTable], var symbols: List[Symbol]) {
    def this() = this(None, List())

    /**
     * Adds a symbol to the table
     * Return value is None if the insert succeeded, and returns a Some[Symbol] is the symbol was duplicate.
     * The symbol is considered duplicate if either:
     * - The most local scope contains a field of the same name.
     * - There is a method of the same name in any scope.
     */
    def addSymbol (symbol: Symbol) : Option[Symbol] = {
      // Check there's no local var of same name
      lookupSymbolLocal(byID(symbol.id)) match {
        case Some(s) => return Some(s)
        // Check that there's no method in any scope of the same name
        case None => lookupSymbol(and(byID(symbol.id), isMethod())) match {
          case Some(s) => return Some(s)
          case None => {
            symbols = symbols :+ symbol
            return None;
          }
        }
      }
    }
    
    /**
     * Adds a list of symbols to a table, returns a list of symbols that were found to be duplicates
     */
    def addSymbols (newSymbols: List[Symbol]) : List[Symbol] = {
      var duplicateSymbols: List[Option[Symbol]] = List()
      for (s <- newSymbols) { 
        duplicateSymbols = duplicateSymbols :+ addSymbol(s)
      }
      return duplicateSymbols flatten
    }

    /**
     * Looks up the first field in the current scope that matches the given predicate.
     * For example: table.lookupSymbolLocal(byID("x") and isMethod) tries to find a variable named "x" that is a method.
     */
    def lookupSymbolLocal(pred: LookupPredicate) : Option[Symbol] = {
      for (s <- symbols) {
        if (pred(s)) {
          return Some(s)
        }
      }
      return None
    }

    /**
     * Similar to lookupSymbolLocal, but it also tries looking through the parent scopes.
     */
    def lookupSymbol(pred: LookupPredicate) : Option[Symbol] = {
      lookupSymbolLocal(pred) match {
        case Some(s) => Some(s)
        case None    =>  parent.flatMap(_.lookupSymbol(pred))
      }
    }
  }

  

  // Combines two predicates together into a single one
  def and(a: LookupPredicate, b: LookupPredicate) : LookupPredicate 
    = (s:Symbol) => a(s) && b(s)

  // Matches a symbol matching the given ID
  def byID(id: String) : LookupPredicate = _.id == id

  // Matches a symbol that is a field
  def isField() : LookupPredicate = (s:Symbol) => s match {
    case s:FieldSymbol => true
    case _             => false
  }

  // Matches a symbol that is a method
  def isMethod() : LookupPredicate = (s:Symbol) => s match {
    case s:MethodSymbol => true
    case _              => false
  }

  // Matches a symbol that is a callout
  def isCallout() : LookupPredicate = (s:Symbol) => s match {
    case s:CalloutSymbol => true
    case _               => false
  }

}
