package compile;

object SymbolTable {
  import IR._
  import IRShared._
  import scala.util.control.Breaks._

  /**
   * Represents something that can be looked up in a symbol table.
   */
  sealed abstract trait Symbol {
    def id: ID
  }

  case class CalloutSymbol(id: ID) extends Symbol
  case class FieldSymbol(dtype: DType, id: ID, size: Option[Long]) extends Symbol
  case class MethodSymbol(
    id: ID,
    params: SymbolTable,
    returns: DType,
    var block: Block
    ) extends Symbol

  implicit class EnhancedSymbol(s: Symbol) {
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

  // Automatically converts a MethodSymbol to an EnhancedMethodSymbol in order to add a .args method to method symbols
  implicit class EnhancedMethodSymbol(m: MethodSymbol) {
    /**
      * Gets the list of arguments to this function.
      */
    def args(): List[FieldSymbol] = m.params.symbols.filter(s => s match {
      case f:FieldSymbol => true
      case _             => {assert(false, "Non-field symbol found in method's symbol table."); false}
    }) map (_.asInstanceOf[FieldSymbol])
  }

  implicit class EnhancedFieldSymbol(f: FieldSymbol) {
    def isArray(): Boolean = f.size match {
      case None   => false
      case Some(s) => true
    }
  }

  case class Conflict(first: Symbol, second: Symbol)

  sealed abstract trait Offset {
    def index : Int
  }

  // Represents an offset from the base pointer
  case class LocalOffset(index: Int) extends Offset
  // Represents an offset from the stack pointer
  case class ArgOffset(index: Int) extends Offset
  // Represents an offset into the global vars
  case class GlobalOffset(index: Int) extends Offset

  class SymbolTable (var parent: Option[SymbolTable]) {
    var symbols:List[Symbol] = List();

    def this() = this(None)
    def this(p: SymbolTable) = this(Some(p))

    /**
      * Am I the top-level symbol table?
      */
    def isGlobalTable() : Boolean = parent match {
      case None   => true
      case Some(p) => false
    }

    /**
      * Returns the symbol table representing the global scope.
      */
    def globalTable() : SymbolTable = parent match {
      case None    => this
      case Some(p) => p.globalTable();
    }

    /**
      * Am I a method parameter symbol table?
      */
    def isMethodTable() : Boolean = parent match {
      case None => false // nope, I'm global
      case Some(pa) => pa.parent match {
        case None => true // I've got no grandparent, but I have a parent, so I'm a method table
        case Some(grandpa) => false // I'm too deep in the heirarchy to be a method table
      }
    }

    /**
      * Returns the symbol table containing the args of the current method.
      * This method fails if this table is in fact the global table.
      */
    def methodTable() : Option[SymbolTable] = parent match {
      case None    => None // This is the global table!
      case Some(pa) => pa.parent match {
        case None => Some(this) // If I have a pa, but no grandpa, then I'm a method param table
        case Some(grandpa) => pa.methodTable()
      }
    }

    def isLocalTable() : Boolean = !isMethodTable && !isGlobalTable

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
      import scala.language.postfixOps
      return duplicateSymbols flatten
    }

    /**
     * Looks up the first field in the current scope with the given name
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
      * Looks up a symbol, and also returns the symbol table it was found in
      */
    def lookupSymbolExtra(id: ID) : Option[(Symbol, SymbolTable)] = {
      lookupSymbolLocal(id) match {
        case Some(s) => Some((s, this))
        case None    => parent.flatMap(_.lookupSymbolExtra(id))
      }
    }

    /**
     * Similar to lookupSymbolLocal, but it also tries looking through the parent scopes.
     */
    def lookupSymbol(id: ID) : Option[Symbol] = lookupSymbolExtra(id).map(_._1)

    /**
      * Returns the hierarchy of parent scopes to the current scope which are also local scopes.
      * The first item in the list is the most recent ancestor.
      */
    def parentLocalScopes() : List[SymbolTable] = {
      if (!isLocalTable()) {
        return List()
      }
      parent match {
        case None => List()
        case Some(p) => {
          if (p.isLocalTable()) {
            return p :: p.parentLocalScopes()
          } else {
            return List()
          }
        }
      }
    }

    def varOffset(id: ID) : Offset = lookupSymbolExtra(id) match {
      case Some((field:FieldSymbol, table)) => {
        var offset: Int = 0;
        breakable {for (s <- table.symbols) s match {
          case f:FieldSymbol => {
            if (f == field) break
            offset += f.size.getOrElse(1).asInstanceOf[Int]
          }
          case _ =>
        }}
        if (table.isGlobalTable()) {
          return GlobalOffset(offset);
        }
        else if (table.isMethodTable()) {
          return ArgOffset(offset);
        }
        table.parentLocalScopes.foreach(t => offset += t.size())
        return LocalOffset(offset)
      }
      case Some((_, _)) => assert(false, "Tried to calculate the offset of a callout or a method. This makes no sense."); LocalOffset(0)
      case None => assert(false, "Tried to calculate offset of variable that doesn't exist"); LocalOffset(0)
    }

    // The total number of words occupied by the fields in this scope
    def size() : Int = {
      var s: Int = 0;
      for (f <- symbols) f match {
        case FieldSymbol(_, _, size) => s += size.getOrElse(1).asInstanceOf[Int]
        case _ => // skip non-fields
      }
      return s
    }

    /**
     * Get all symbols a type T.
     */
    def getFields = symbols.map{
      _ match {
        case s: FieldSymbol => Some(s)
        case _ => None
      }
    }.flatten

    def copy() : SymbolTable = {
      val table = new SymbolTable(parent)
      table.symbols = symbols
      return table
    }

    override
    def toString: String =
      "SymbolTable(%s, parent=%s)".format(symbols.map(_.id), parent)

    def prettyprint: String =
      "SymbolTable(%s, parent=%s)".format(symbols, parent)

  }
}
