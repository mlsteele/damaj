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

  class EnhancedSymbol(s: Symbol) {
    def isField(): Boolean = s match {
      case f:FieldSymbol => true
      case _ => false
    }

    def isMethod(): Boolean = s match {
      case m:MethodSymbol => true
      case _ => false
    }

    def isCallout(): Boolean = s match {
      case c:CalloutSymbol => true
      case _ => false
    }

    def dtype(): DType = s match {
      case f:FieldSymbol => f.dtype
      case m:MethodSymbol => m.returns
      case c:CalloutSymbol => DTInt
    }
  }

  implicit def enhanceSymbol(s:Symbol) = new EnhancedSymbol(s)

  class EnhancedMethodSymbol(m: MethodSymbol) {
    /**
      * Gets the list of arguments to this function.
      */
    def args(): List[FieldSymbol] = m.params.symbols.filter(s => s match {
      case f:FieldSymbol => true
      case _             => {assert(false, "Non-field symbol found in method's symbol table."); false}
    }) map (_.asInstanceOf[FieldSymbol])
  }

  // Automatically converts a MethodSymbol to an EnhancedMethodSymbol in order to add a .args method to method symbols
  implicit def enhancedMethodSymbol(m: MethodSymbol) = new EnhancedMethodSymbol(m)

  case class Conflict(first: Symbol, second: Symbol)

  class SymbolTable (var parent: Option[SymbolTable]) {

    var symbols:List[Symbol] = List();

    def this() = this(None)
    def this(p: SymbolTable) = this(Some(p))

    /**
      * Returns the symbol table representing the global scope.
      */
    def globalScope() : SymbolTable = parent match {
      case None    => this
      case Some(p) => p.globalScope();
    }

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

    override
    def toString: String = "SymbolTable(%s, parent=%s)"
      .format(symbols.map(_.id), parent)
  }
}
