package compile 

// namespace collisions import AST._
// namespace collisions import IR._
//
// Code primarily borromed from ASTTools.scala.
// variable names come from the naming convention 
// established there
//
// TODO: put callouts in symbol table
//Construct an IR from a AST
object IRBuilder{
  import SymbolTable._
  import IRShared._
  class IRConstructionException(msg: String) extends RuntimeException(msg)

  def convertProgram(ast: AST.ProgramAST): IR.ProgramIR = {
    val symbols = new SymbolTable()
    val duplicate_callouts = symbols.addSymbols(ast.callouts.map(x => CalloutSymbol(x.id)))
    assert(duplicate_callouts.length == 0, "TODO error reporting" + duplicate_callouts)

    val duplicate_fields = symbols.addSymbols(ast.fields.map(x => FieldSymbol(x.dtype, x.id, x.size)))
    assert(duplicate_fields.length == 0, "TODO error reporting" + duplicate_fields)

    val duplicate_methods = symbols.addSymbols(ast.methods.map(convertMethodDecl(_,symbols)))
    assert(duplicate_methods.length == 0, "TODO error reporting" + duplicate_methods)

    // Verify void main() exists
    symbols.lookupSymbol("main") match {
      case Some(s) => s match {
        case MethodSymbol(id, params, returns, block) => returns match {
          case DTVoid => IR.ProgramIR(symbols) // OK. TODO check arguments
          case _ => 
            assert(false, "Method `main` must return void")
            IR.ProgramIR(symbols)
        }
        case _ =>
          assert(false, "No method `main` found")
          IR.ProgramIR(symbols)
      }
      case _ => 
        assert(false, "No method `main` found")
        IR.ProgramIR(symbols)
    }
  }

  def convertFieldDecl(ast: AST.FieldDecl, symbols:SymbolTable): FieldSymbol = FieldSymbol(ast.dtype, ast.id, ast.size)

  def convertMethodDecl(meth: AST.MethodDecl, parent: SymbolTable): MethodSymbol = {
    val paramsTable = new SymbolTable(parent)
    val duplicate_args = paramsTable.addSymbols(meth.args.map(convertMethodDeclArg(_, parent)))
    assert(duplicate_args.length == 0, "TODO error reporting" + duplicate_args)
    return MethodSymbol(meth.id, paramsTable, meth.returns, convertBlock(meth.block, paramsTable,false))
  }

  // Only used after crashing because of an error
  val dummyExpr = IR.LoadLiteral(BoolLiteral(false))
     
  def convertExpr(expr: AST.Expr, symbols:SymbolTable): IR.Expr = expr match {
    case AST.BinOp(left, op, right) => verifyExpr(IR.BinOp(convertExpr(left, symbols), op, convertExpr(right, symbols)))
    case AST.UnaryOp(op, right) => verifyExpr(IR.UnaryOp(op, convertExpr(right, symbols)))
    case AST.Ternary(condition, left, right) => 
      verifyExpr(IR.Ternary(convertExpr(condition, symbols), convertExpr(left, symbols), convertExpr(right, symbols)))
    case AST.Literal(l) => verifyExpr(IR.LoadLiteral(l))
    case AST.Location(id, index) => 
      val fs = symbols.lookupSymbol(id)
      fs match {
        case Some(s) => s match {
          case f:FieldSymbol => verifyExpr(IR.LoadField(f, convertExpr(index, symbols)))
          case _ =>
            assert(false, "Location must be a field")
            dummyExpr
        }
        case _ =>
          assert(false, "Unknown identifier")
          dummyExpr
      }
    case a:AST.MethodCall => convertMethodCall(a, symbols,false) match {
      case Some(x) => verifyExpr(x)
      case None => assert(false, "Unknown identifier"); dummyExpr
    }
  }
  
  // Helper for converting Option[Expr]'s. If you don't already have an Option, don't use this
  def convertExpr(oexpr: Option[AST.Expr], symbols:SymbolTable): Option[IR.Expr] = oexpr match {
    case Some(expr) => Some(convertExpr(expr, symbols))
    case None => None
  }

  def typeOfExpr(expr: IR.Expr): DType = expr match {
    case IR.BinOp(l,o,r) => o match {
      case "+"|"-"|"*"|"/"|"%" => DTInt
      case "&&"|"||"|"<"|">"|"=="|"!="|"<="|">=" => DTBool
    }
    case IR.UnaryOp(o,r) => o match {
      case "@"|"-" => DTInt
      case "!" => DTBool
    }
    case IR.Ternary(c, l, r) => typeOfExpr(l)
    case IR.LoadField(f,i) => f.dtype
    case IR.LoadLiteral(i) => i match {
      case i:IntLiteral => DTInt
      case c:CharLiteral => DTInt
      case b:BoolLiteral => DTBool
    }
    case IR.Store(t,i) => t.dtype
    case IR.MethodCall(method, args) => method.returns
    case IR.CalloutCall(callout, args) => DTInt

  }

  def verifyExpr(expr: IR.Expr): IR.Expr = expr match {
    case IR.BinOp(l,o,r) => o match {
      case "=="|"!=" =>
        if (typeOfExpr(l) != typeOfExpr(r)) {
          assert(false, "Mismatched types on == or !=")
          dummyExpr
        } else {
          expr
        }
      case "+"|"-"|"*"|"/"|"%"|"<"|">"|"<="|">=" =>
        if (typeOfExpr(l) != DTInt || typeOfExpr(r) != DTInt) {
          assert (false, "Mathematical operation requires ints")
          dummyExpr
        } else {
          expr
        }
      case "&&"|"||" =>
        if (typeOfExpr(l) != DTBool || typeOfExpr(r) != DTBool) {
          assert(false, "Logical expression requires booleans")
          dummyExpr
        } else {
          expr
        }
    }
    case IR.UnaryOp(o,r) => o match {
      case "!" =>
        if (typeOfExpr(r) != DTBool) {
          assert(false, "! operator must act on a boolean expression")
          dummyExpr
        } else {
          expr
        }
      case "@" => r match {
        case IR.LoadField(fs, oi) => oi match {
          case Some(i) => expr // good
          case None => 
            assert(false, "@ operator must act on an array")
            dummyExpr
        }
        case _ =>
          assert(false, "@ operator must act on a field")
          dummyExpr
      }
      case "-" => typeOfExpr(r) match {
        case DTInt =>
          expr
        case _ =>
          assert(false, "- operator must act on an integer expression")
          dummyExpr
      }
    }
    case IR.Ternary(c,l,r) => typeOfExpr(c) match {
      case DTBool =>
        if (typeOfExpr(l) == typeOfExpr(r)) {
          expr
        } else {
          assert(false, "Second and third parts of ternary must match in type")
          dummyExpr
        }
      case _ =>
        assert(false, "Ternary condition must be boolean")
        dummyExpr
    }
    case IR.LoadField(fs, oi) => oi match {
      case Some(i) => fs.size match {
        case Some(int) => expr
        case None => 
          assert(false, "Brackets used on non-array type")
          dummyExpr
      }
      case None => expr
    }
    // TODO(jessk,diony): Check the number and type of arguments
    case IR.MethodCall(ms, args) => expr
    
    case _ => expr
    
  }

  // TODO
  // def convertID()
  def convertMethodDeclArg(ast: AST.MethodDeclArg, symbols:SymbolTable): FieldSymbol = FieldSymbol(ast.dtype, ast.id, None)

  val dummyStatement = IR.Continue
  def convertStatement(ast:AST.Statement, symbols:SymbolTable,inLoop:Boolean): IR.Statement = ast match{
    case a:AST.Assignment => convertAssignment(a, symbols)
    // TODO uncomment/comment these and uncomment convertAssignment when you're ready to work on it.
    case a:AST.MethodCall => convertMethodCall(a, symbols,false) match {
      case Some(x) => x
      case None => assert(false, "Method does not exist " + a.id)
      dummyStatement
    }
    case a:AST.If => convertIf(a, symbols,inLoop)
    case a:AST.For => convertFor(a, symbols,inLoop)
    case a:AST.While => convertWhile(a, symbols,true)
    case a:AST.Return => convertReturn(a, symbols,true)
    case AST.Break if inLoop => IR.Break
    case AST.Break => assert(false,"break must be in a loop")
                      IR.Break // TODO (Andres): change this once error fixing 
    case AST.Continue if inLoop=> IR.Continue
    case AST.Continue => assert(false,"continue must be in a loop"); IR.Continue //IR.Continue
  }

  def convertMethodCall(ast: AST.MethodCall, symbols:SymbolTable,inLoop:Boolean): Option[IR.Call] = {
    val symbol = symbols.lookupSymbol(ast.id)
    println(symbols)
    println("FOOO")
    symbol match {
      case Some(s) => s match {
        case m:MethodSymbol => Some(IR.MethodCall(m, ast.args.map(convertMethodCallArg(_, symbols))))
        case c:CalloutSymbol => Some(IR.CalloutCall(c, ast.args.map(convertMethodCallArg(_, symbols))))
        case f:FieldSymbol => None 
      }
      case None => None
    }
  }

  def convertMethodCallArg(ast: Either[StrLiteral, AST.Expr], symbols:SymbolTable): Either[StrLiteral, IR.Expr] = ast match {
    case Left(x) => Left(x)
    case Right(x) => Right(convertExpr(x, symbols))
  }

  def convertAssignment(assign: AST.Assignment, symbols:SymbolTable): IR.Assignment =
    IR.Assignment(locToStore(assign.left, symbols), convertExpr(assign.right, symbols))

  def locToStore(loc: AST.Location, symbols:SymbolTable): IR.Store = {
    val table = new SymbolTable()
    val field = table.lookupSymbol(loc.id)
    field match{
      case Some(f)=> f match{
        case f:FieldSymbol => IR.Store(f, convertExpr(loc.index,symbols))
        case _ =>
          assert(false, "Cannot assign to non-field")
          IR.Store(FieldSymbol(DTVoid,"void",None),Some(dummyExpr))// TODO (Andres): replace this dummy error with an actual one
      }
        case None =>
          assert(false, "Must declare field before assignment")
          IR.Store(FieldSymbol(DTVoid,"void",None),Some(dummyExpr)) //TODO (Andres:) replace this dummy error with an actual one
   
    }
  }
  def convertBlock(block: AST.Block, symbols:SymbolTable,inLoop:Boolean): IR.Block = {
    val localtable = new SymbolTable(Some(symbols))
    val duplicates = localtable.addSymbols(block.decls.map(x => FieldSymbol(x.dtype, x.id, x.size)))
    assert(duplicates.length == 0, "Duplicate field declarations " + duplicates)
    // TODO enter symbols into table
    
    
      IR.Block(block.stmts.map(convertStatement(_, localtable,inLoop)), localtable)
  
    
  }
  // TODO these just return dummies for now. Comment/Uncomment when you start working on these.
  // def convertIf(iff: AST.If): IR.If = IR.If(convertExpr(iff.condition), convertBlock(iff.then), iff.elseb.map(convertBlock))
  // def convertFor(fo: AST.For): IR.For = IR.For(fo.id, convertExpr(fo.start), convertExpr(fo.iter), convertBlock(fo.then))
  // def convertWhile (whil:AST.While): IR.While = IR.While(convertExpr(whil.condition), convertBlock(whil.block), whil.max)
  // def convertReturn (ret:AST.Return): IR.Return = IR.Return(ret.expr.map(convertExpr))
  def convertIf(iff: AST.If, symbols:SymbolTable,inLoop:Boolean): IR.Statement = IR.Break
  def convertFor(fo: AST.For, symbols:SymbolTable,inLoop:Boolean): IR.Statement = IR.Break
  def convertWhile (whil:AST.While, symbols:SymbolTable,inLoop:Boolean): IR.Statement = IR.Break
  def convertReturn (ret:AST.Return, symbols:SymbolTable,inLoop:Boolean): IR.Statement = IR.Break

  //  def convertBreak ( ) this can be done through =>
  //  def convertContinue ( ) this can be done through =>
  //  convertMany[AST.CalloutDecl](ast, "callout_many", convertCalloutDecl)
}
