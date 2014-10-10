package compile

object TempVarGen {
  import SymbolTable._
  import IRShared._

  /*
   * Takes a symbol table and progressively inserts automatically
   * named symbols into the table as requested.
   */
  class TempVarGen (var table: SymbolTable, prefix: String) {
    var counter: Int = 0

    def this(t: SymbolTable) = this(t, "_t")

    private def newVarName() : ID = {
      val varName: String = prefix + counter;
      counter += 1;
      table.lookupSymbol(varName) match {
        case None => return varName
        case _    => return newVarName()
      }
    }

    // Generates a new variable of the given type
    def newVar(dtype: DType) : FieldSymbol = {
      val sym = FieldSymbol(dtype, newVarName(), None)
      table.addSymbol(sym) match {
        case None => return sym
        case _    => assert(false, "Duplicate temporary variable created somehow."); return sym
      }
    }

    //  Generates a new int variable
    def newIntVar() : FieldSymbol = newVar(DTInt)

    // Generates a new bool variable
    def newBoolVar() : FieldSymbol = newVar(DTBool)

  }
}
