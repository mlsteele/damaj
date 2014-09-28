package compile;

import IR._

object SymbolTable {
  sealed abstract trait Symbol {
    def id: ID
  }
  case class CalloutSymbol(id: ID) extends Symbol
  case class FieldSymbol(id: ID, dType: DType, size: Option[IntLiteral]) extends Symbol
  case class MethodSymbol(
    id: ID,
    params: SymbolTable,
    returns: DType,
    block: Block
    ) extends Symbol

  type LookupPredicate = Symbol => Boolean

  class SymbolTable (val parent: Option[SymbolTable], val symbols: List[Symbol]) {
    // Adds a symbol to the table, and returns a tuple of (new table, )
    def addSymbol (symbol: Symbol) : (SymbolTable, Option[Symbol]) = {
      if (symbols.contains(symbols)) {
        return (this, Some(symbol))
      } else {
        return (new SymbolTable(parent, symbol +: symbols), None);
      }
    }
    
    // Adds a list of symbols to a table, and returns a new table, and a list of symbols that were found to be duplicates
    def addSymbols (newSymbols: List[Symbol]) : (SymbolTable, List[Symbol]) = {
      var duplicateSymbols: List[Option[Symbol]] = List()
      var newTable: SymbolTable = this
      for (s <- newSymbols) { 
        val (tempTable, duplicated) = newTable.addSymbol(s);
        newTable = tempTable
        duplicateSymbols = duplicateSymbols :+ duplicated
      }
      return (this, duplicateSymbols flatten)
    }

    // Similar to lookupSymbol, but it does not recurse through its parents
    def lookupSymbolLocal(pred: LookupPredicate) : Option[Symbol] = {
      for (s <- symbols) {
        if (pred(s)) {
          return Some(s)
        }
      }
      return None
    }

    // Returns the first symbol that matches the given predicate, recursing through its parent to find the symbol if needed
    def lookupSymbol(pred: LookupPredicate) : Option[Symbol] = {
      lookupSymbolLocal(pred) match {
        case Some(s) => Some(s)
        case None    =>  parent.flatMap(_.lookupSymbol(pred))
      }
    }
  }

  // Combines two predicates together
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
