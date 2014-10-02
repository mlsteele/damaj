package compile 

// namespace collisions import AST._
// namespace collisions import IR._
//
// Code primarily borromed from ASTTools.scala.
// variable names come from the naming convention 
// established there
//
// Example Usage:
// IRBuilder(parseTree).ir match {
//   case Left(errors) => weep
//   case Right(ir) => rejoice
// }
//
// TODO: put callouts in symbol table
//Construct an IR from a AST
class IRBuilder(input: AST.ProgramAST) {
  import collection.mutable.ListBuffer
  import SymbolTable._
  import IRShared._

  class IRConstructionException(msg: String) extends RuntimeException(msg)
  type SemanticError = String

  val srcmap = input.srcmap
  // Keep track of errors during construction
  // The presence of errors in this list will cause
  // ir to hold a failure.
  val errors = ListBuffer[SemanticError]()
  val ir: Either[List[SemanticError], IR.ProgramIR] = convertProgram(input)

  def convertProgram(ast: AST.ProgramAST): Either[List[SemanticError], IR.ProgramIR] = {
    val symbols = new SymbolTable()

    // callouts
    errors ++= symbols
      .addSymbols(ast.callouts.map(x => srcmap.alias(x, CalloutSymbol(x.id))))
      .map{ conflict =>
        srcmap.report(conflict.second,
          "Duplicate callout declaration '%s'.".format(conflict.second.id))
      }

    // fields
    errors ++= symbols
      .addSymbols(ast.fields.map(convertFieldDecl(_, symbols)))
      .map{ conflict => "TODO better error reporting" }

    // methods
    errors ++= ast.methods
      .map{ m => symbols.addSymbol(convertMethodDecl(m, symbols)) }
      .flatten.map{ conflict => "TODO better error reporting" }

    val unchecked_ir = IR.ProgramIR(symbols)
    // post-process checks
    checkMainMethod(unchecked_ir)

    // check the errors list to see whether an ir would be valid.
    errors.toList match {
      case Nil => Right(IR.ProgramIR(symbols))
      case errors => Left(errors)
    }
  }

  // Make sure the the main method exists.
  // And has the correct signature. (void main())
  // Adds stuff to errors if there are issues.
  def checkMainMethod(unchecked_ir: IR.ProgramIR): Unit = {
    // Verify main exists
    unchecked_ir.symbols.lookupSymbol("main") match {
      case Some(m: MethodSymbol) =>
        // Check arguments
        m.args match {
          case List() => // ok.
          case _ => errors += "Method `main` must take no arguments."
        }
        // Check return type
        m.returns match {
          case DTVoid => // ok.
          case _ => // maybe this ok too? TODO(miles): resolve ambiguity.
        }
      case _ => errors += "No method `main` found"
    }
  }

  def convertFieldDecl(ast: AST.FieldDecl, symbols:SymbolTable): FieldSymbol = {
    printf("Converting field decl " + ast)
    ast.size match {
      case Some(s) => assert(s.value > 0, "Array must have size greater than 0")
      case _ => // ok
    }
    FieldSymbol(ast.dtype, ast.id, ast.size)
  }

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
    case a:AST.MethodCall => convertMethodCall(a, symbols) match {
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

  def convertMethodDeclArg(ast: AST.MethodDeclArg, symbols:SymbolTable): FieldSymbol =
    FieldSymbol(ast.dtype, ast.id, None)

  def convertID(id: ID, symbols:SymbolTable) = symbols.lookupSymbol(id) match {
    case Some(s) => s match {
      case f:FieldSymbol => IR.LoadField(f, None)
      case _ =>
        assert(false, "Location must be a field")
        dummyExpr
    }
    case _ =>
      assert(false, "Unknown identifier")
      dummyExpr
  }


  def convertStatement(ast:AST.Statement, symbols:SymbolTable, inLoop:Boolean): Option[IR.Statement] = ast match{
    /// TODO(miles): remove ALL of these Some's in favor of error-determined options.
    case a: AST.Assignment => Some(convertAssignment(a, symbols))
    case a: AST.MethodCall => convertMethodCall(a, symbols)
    case a: AST.If => convertIf(a, symbols, inLoop)
    case a: AST.For => convertFor(a, symbols)
    case a: AST.While => convertWhile(a, symbols)
    case a: AST.Return => convertReturn(a, symbols)
    case AST.Break if inLoop => Some(IR.Break)
    case AST.Break => assert(false,"break must be in a loop")
                      None
    case AST.Continue if inLoop => Some(IR.Continue)
    case AST.Continue => assert(false, "continue must be in a loop");
                         None
  }

  def convertMethodCall(ast: AST.MethodCall, symbols:SymbolTable): Option[IR.Call] = {
    val oSymbol = symbols.lookupSymbol(ast.id)
    println(symbols)
    println("FOOO")
    oSymbol match {
      case Some(s) => s match {
        case symbol:MethodSymbol => 
          val methodCallArgs = ast.args.map(convertMethodCallArg(_, symbols))
          assert(methodCallArgs.length == symbol.args.length, "Method called with wrong number of arguments")
          (symbol.args zip methodCallArgs) map {
            case (f:FieldSymbol, Right(expr)) =>
              assert(f.dtype == typeOfExpr(expr), "Method called with the wrong argument types")
            case (f:FieldSymbol, Left(expr)) =>
              assert(false, "Method cannot be called with a string literal argument")
          }
          Some(IR.MethodCall(symbol, methodCallArgs))
        case c:CalloutSymbol => Some(IR.CalloutCall(c, ast.args.map(convertMethodCallArg(_, symbols))))
        case f:FieldSymbol => 
          assert(false, "Cannot call a field")
          None 
      }
      case None => 
        assert(false, "Name %s not found in %s".format(ast.id, symbols))
        None
    }
  }

  def convertMethodCallArg(ast: Either[StrLiteral, AST.Expr], symbols:SymbolTable): Either[StrLiteral, IR.Expr] = ast match {
    case Left(x) => Left(x)
    case Right(x) => Right(convertExpr(x, symbols))
  }

  // TODO(miles): Does this check for matching types yet?
  def convertAssignment(assign: AST.Assignment, symbols:SymbolTable): IR.Assignment =
    IR.Assignment(locToStore(assign.left, symbols), convertExpr(assign.right, symbols))

  // TODO(miles): Does this make sur ethat that the left is not an array?
  def locToStore(loc: AST.Location, symbols:SymbolTable): IR.Store = {
    val field = symbols.lookupSymbol(loc.id)
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

  def convertBlock(block: AST.Block, symbols:SymbolTable, inLoop:Boolean): IR.Block = {
    val localtable = new SymbolTable(Some(symbols))
    val duplicates = localtable.addSymbols(block.decls.map(x => FieldSymbol(x.dtype, x.id, x.size)))
    assert(duplicates.length == 0, "Duplicate field declarations " + duplicates)
    // TODO enter symbols into table
    // flatten is used here to drop the None's from the stmt list.
    val stmts = block.stmts.map(convertStatement(_, localtable, inLoop)).flatten
    IR.Block(stmts, localtable)
  }

  def convertIf(iff: AST.If, symbols: SymbolTable, inLoop: Boolean): Option[IR.If] = {
    // TODO(miles): report errors in blocks AFTER error in condition statement.
    val condition: IR.Expr = convertExpr(iff.condition, symbols)
    val thenBlock: IR.Block = convertBlock(iff.then, symbols, inLoop)
    val elseBlock: Option[IR.Block] = iff.elseb.map(convertBlock(_, symbols, inLoop))

    typeOfExpr(condition) match {
      case DTBool => Some(IR.If(condition, thenBlock, elseBlock))
      case _ =>
        errors += srcmap.report(iff.condition, "If condition must be a boolean")
        None
    }
  }

<<<<<<< HEAD
  // TODO implement convertFor
  def convertFor(fo: AST.For, symbols: SymbolTable): Option[IR.Statement] =
    Some(IR.Break)
=======
  def convertFor(fo: AST.For, symbols:SymbolTable,inLoop:Boolean): Option[IR.Statement] = symbols.lookupSymbol(fo.id) match {
    case None => assert(false, "Symbol not found."); None
    case Some(field) => field match {
      case f:FieldSymbol => {
        f.size match {
          case Some(s) => assert(false, "Cannot use an array as a for-loop variable"); return None
          case None =>
        }
        if (f.dtype != DTInt) {assert(false, "Must loop over a variable."); return None}
        val start:IR.Expr = convertExpr(fo.start, symbols)
        if (typeOfExpr(start) != DTInt) {assert(false, "Loop start value must be an int."); return None}
        val end:IR.Expr = convertExpr(fo.iter, symbols)
        if (typeOfExpr(end) != DTInt) {assert(false, "Loop end value must be an int."); return None}
        val block:IR.Block = convertBlock(fo.then, symbols, inLoop)
        return Some(IR.For(fo.id, start, end, block))
      }
      case _ => {assert(false, "Loop variable is not a field."); return None}
    }
  }
>>>>>>> Implemented convertFor.

  def convertWhile(whil: AST.While, symbols: SymbolTable): Option[IR.While] = {
    // TODO(miles): report errors in blocks AFTER error in condition statement.
    val cond = convertExpr(whil.condition, symbols)
    val block = convertBlock(whil.block, symbols, true)

    typeOfExpr(cond) match {
      case DTBool => Some(IR.While(cond, block, whil.max))
      case _ =>
        errors += srcmap.report(whil.condition, "While condition must be a boolean")
        None
    }
  }

  // TODO implement convertReturn
  def convertReturn (ret:AST.Return, symbols:SymbolTable): Option[IR.Statement] =
    Some(IR.Break)
}
