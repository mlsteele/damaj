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
      for (f <- newSymbols) { 
        val (tempTable, duplicated) = newTable.addSymbol(f);
        newTable = tempTable
        duplicateSymbols = duplicateSymbols :+ duplicated
      }
      return (this, duplicateSymbols flatten)
    }

    // Looks up a symbol, and recurses up though its parents to find the symbol if needed
    def lookupSymbol (id: ID) : Option[Symbol] = {
      for (s <- symbols) {
        if (s.id == id) {
          return Some(s);
        }
      }
      // If we got to here, then we need to recurse through out parent
      parent.flatMap(_.lookupSymbol(id))
    }
  }
}
