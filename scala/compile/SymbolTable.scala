package compile;

import IR._

object SymbolTable {
  // placeholder until someone writes the real symbol tables
  class SymbolTable (
    val parent: Option[SymbolTable],
    val fields: List[Field]
  ) {
    // Adds a field to the table, and returns a tuple of (new table, )
    def addField (field: Field) : (SymbolTable, Option[Field]) = {
      if (fields.contains(field)) {
        return (this, Some(field))
      } else {
        return (new SymbolTable(parent, field +: fields), None);
      }
    }
    
    // Adds a list of fields to a table, and returns a new table, and a list of fields that were found to be duplicates
    def addFields (newFields: List[Field]) : (SymbolTable, List[Field]) = {
      var duplicateFields: List[Option[Field]] = List()
      var newTable: SymbolTable = this
      for (f <- newFields) { 
        val (tempTable, duplicated) = newTable.addField(f);
        newTable = tempTable
        duplicateFields = duplicateFields :+ duplicated
      }
      return (this, duplicateFields flatten)
    }

    // Looks up a symbol, and recurses up though its parents to find the symbol if needed
    def lookupSymbol (id: ID) : Option[Field] = {
      for (f <- fields) {
        if (f.id == id) {
          return Some(f);
        }
      }
      // If we got to here, then we need to recurse through out parent
      parent.flatMap(_.lookupSymbol(id))
    }
  }
}
