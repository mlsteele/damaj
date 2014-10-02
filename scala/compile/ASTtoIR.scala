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

  case class Context(symbols:SymbolTable, inLoop:Boolean, returnType:DType)

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
    addFieldsToTable(symbols, ast.fields)

    // methods
    // TODO(miles)
    // convetMethodDecl already adds itself to the symbol table, fix error reporting
    ast.methods
      .map(convertMethodDecl(_, Context(symbols, false, DTVoid)))

    val unchecked_ir = IR.ProgramIR(symbols)
    // post-process checks
    checkMainMethod(unchecked_ir)

    // check the errors list to see whether an ir would be valid.
    errors.toList match {
      case Nil => Right(IR.ProgramIR(symbols))
      case errors => Left(errors)
    }
  }

  // Adds fields to table.
  // Adds errors about poorly constructed fields.
  // Adds errors about conflicts.
  def addFieldsToTable(symbols: SymbolTable, fields: List[AST.FieldDecl]): Unit = {
    errors ++= fields.map{ fd =>
      convertFieldDecl(fd) match {
        case Some(fs) => symbols.addSymbol(fs)
        case None => None
      }
    }.flatten.map{
      conflict => "TODO better error reporting (double var)"
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

  def convertFieldDecl(ast: AST.FieldDecl): Option[FieldSymbol] = {
    ast.size match {
      case Some(lit) => convertIntLiteral(lit) match {
        case None => None
        case Some(v) if v <= 0 =>
          errors += srcmap.report(ast, "Array field must have length > 0")
          None
        case Some(v) => Some(FieldSymbol(ast.dtype, ast.id, Some(v)))
      }
      case None => Some(FieldSymbol(ast.dtype, ast.id, None))
    }
  }

  def convertIntLiteral(ast: IntLiteral): Option[Long] = ast.value match {
    case v if v < Long.MinValue =>
      errors += srcmap.report(ast, "Int literal is too damn negative.")
      None
    case v if v > Long.MaxValue =>
      errors += srcmap.report(ast, "Int literal is too damn big.")
      None
    case v => Some(v.toLong)
  }

  def convertMethodDecl(meth: AST.MethodDecl, ctx:Context): MethodSymbol = {
    val paramsTable = new SymbolTable(ctx.symbols)
    val duplicate_args = paramsTable.addSymbols(meth.args.map(convertMethodDeclArg(_, ctx)))
    assert(duplicate_args.length == 0, "TODO error reporting" + duplicate_args)

    //  partialy construct the method so we can insert it into the symbol table
    // this is neccessary in order for the block to be able to refer to the method
    val partialMethod = MethodSymbol(meth.id, paramsTable, meth.returns, IR.Block(List(), paramsTable));

    ctx.symbols.addSymbol(partialMethod) match {
      case None => {
        val childContext = Context(paramsTable, false, meth.returns)
        partialMethod.block = convertBlock(meth.block, childContext)
        return partialMethod
      }
      case _ => assert(false, "NOOOOO"); return partialMethod
    }
  }

  // Only used after crashing because of an error
  val dummyExpr = IR.LoadLiteral(BoolLiteral(false))
     
  def convertExpr(expr: AST.Expr, ctx:Context): IR.Expr = expr match {
    case AST.BinOp(left, op, right) => verifyExpr(IR.BinOp(convertExpr(left, ctx), op, convertExpr(right, ctx)))
    case AST.UnaryOp(op, right) => verifyExpr(IR.UnaryOp(op, convertExpr(right, ctx)))
    case AST.Ternary(condition, left, right) => 
      verifyExpr(IR.Ternary(convertExpr(condition, ctx), convertExpr(left, ctx), convertExpr(right, ctx)))
    case AST.Literal(l) => verifyExpr(IR.LoadLiteral(l))
    case AST.Location(id, index) => 
      val fs = ctx.symbols.lookupSymbol(id)
      fs match {
        case Some(s) => s match {
          case f:FieldSymbol => verifyExpr(IR.LoadField(f, convertExpr(index, ctx)))
          case _ =>
            assert(false, "Location must be a field")
            dummyExpr
        }
        case _ =>
          assert(false, "Unknown identifier")
          dummyExpr
      }
    case a:AST.MethodCall => convertMethodCall(a, ctx) match {
      case Some(x) => verifyExpr(x)
      case None => assert(false, "Unknown identifier"); dummyExpr
    }
  }
  
  // Helper for converting Option[Expr]'s. If you don't already have an Option, don't use this
  def convertExpr(oexpr: Option[AST.Expr], ctx:Context): Option[IR.Expr] = oexpr match {
    case Some(expr) => Some(convertExpr(expr, ctx))
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
        case IR.LoadField(fs, oi) => fs.size match {
          case Some(x)=> expr
          case None => assert(false, "@ operator must act an an array")
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
        case Some(int) => typeOfExpr(expr) match {
          case DTInt => expr
          case _ =>
            assert(false, "Array index must be type int")
            dummyExpr
        }
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

  def convertMethodDeclArg(ast: AST.MethodDeclArg, ctx:Context): FieldSymbol =
    FieldSymbol(ast.dtype, ast.id, None)

  def convertID(id: ID, ctx:Context) = ctx.symbols.lookupSymbol(id) match {
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


  def convertStatement(ast:AST.Statement, ctx:Context): Option[IR.Statement] = ast match{
    /// TODO(miles): remove ALL of these Some's in favor of error-determined options.
    case a: AST.Assignment => Some(convertAssignment(a, ctx))
    case a: AST.MethodCall => convertMethodCall(a, ctx)
    case a: AST.If => convertIf(a, ctx)
    case a: AST.For => convertFor(a, ctx)
    case a: AST.While => convertWhile(a, ctx)
    case a: AST.Return => convertReturn(a, ctx)
    case AST.Break if ctx.inLoop => Some(IR.Break)
    case AST.Break => assert(false,"break must be in a loop")
                      None
    case AST.Continue if ctx.inLoop => Some(IR.Continue)
    case AST.Continue => assert(false, "continue must be in a loop");
                         None
  }

  def convertMethodCall(ast: AST.MethodCall, ctx:Context): Option[IR.Call] = {
    val oSymbol = ctx.symbols.lookupSymbol(ast.id)
    println(ctx.symbols)
    println("FOOO")
    oSymbol match {
      case Some(s) => s match {
        case symbol:MethodSymbol => 
          val methodCallArgs = ast.args.map(convertMethodCallArg(_, ctx))
          assert(methodCallArgs.length == symbol.args.length, "Method called with wrong number of arguments")
          (symbol.args zip methodCallArgs) map {
            case (f:FieldSymbol, Right(expr)) =>
              assert(f.dtype == typeOfExpr(expr), "Method called with the wrong argument types")
            case (f:FieldSymbol, Left(expr)) =>
              assert(false, "Method cannot be called with a string literal argument")
          }
          Some(IR.MethodCall(symbol, methodCallArgs))
        case c:CalloutSymbol => Some(IR.CalloutCall(c, ast.args.map(convertMethodCallArg(_, ctx))))
        case f:FieldSymbol => 
          assert(false, "Cannot call a field")
          None 
      }
      case None => 
        assert(false, "Name %s not found in %s".format(ast.id, ctx.symbols))
        None
    }
  }

  def convertMethodCallArg(ast: Either[StrLiteral, AST.Expr], ctx:Context): Either[StrLiteral, IR.Expr] = ast match {
    case Left(x) => Left(x)
    case Right(x) => Right(convertExpr(x, ctx))
  }

  def convertAssignment(assign: AST.Assignment, ctx:Context): IR.Assignment = {
    val store = locToStore(assign.left, ctx)
    val rhs = convertExpr(assign.right, ctx)
    store.index match {
      // normal variable assignment
      case None => {
        assert(store.to.dtype == typeOfExpr(rhs), "Assignment left and right sides must be of the same type.");
      }
      // Array access
      case Some(i) => {
        assert(typeOfExpr(i) == DTInt, "Array index must be an integer.")
        assert(store.to.isArray, "Cannot index into a scalar.")
      }
    }
    return IR.Assignment(store, rhs)
  }

  // TODO(miles): Does this make sur ethat that the left is not an array?
  def locToStore(loc: AST.Location, ctx:Context): IR.Store = {
    val field = ctx.symbols.lookupSymbol(loc.id)
    field match{
      case Some(f)=> f match{
        case f:FieldSymbol => IR.Store(f, convertExpr(loc.index, ctx))
        case _ =>
          assert(false, "Cannot assign to non-field")
          IR.Store(FieldSymbol(DTVoid,"void",None),Some(dummyExpr))// TODO (Andres): replace this dummy error with an actual one
      }
        case None =>
          assert(false, "Field not found in %s".format(ctx.symbols))
          IR.Store(FieldSymbol(DTVoid,"void",None),Some(dummyExpr)) //TODO (Andres:) replace this dummy error with an actual one
   
    }
  }

  def convertBlock(block: AST.Block, ctx:Context): IR.Block = {
    val localtable = new SymbolTable(ctx.symbols)

    addFieldsToTable(localtable, block.decls)

    // flatten is used here to drop the None's from the stmt list.
    val stmts = block.stmts.map(convertStatement(_, Context(localtable, ctx.inLoop, ctx.returnType))).flatten
    IR.Block(stmts, localtable)
  }

  def convertIf(iff: AST.If, ctx:Context): Option[IR.If] = {
    // TODO(miles): report errors in blocks AFTER error in condition statement.
    val condition: IR.Expr = convertExpr(iff.condition, ctx)
    val thenBlock: IR.Block = convertBlock(iff.then, ctx)
    val elseBlock: Option[IR.Block] = iff.elseb.map(convertBlock(_, ctx))

    typeOfExpr(condition) match {
      case DTBool => Some(IR.If(condition, thenBlock, elseBlock))
      case _ =>
        errors += srcmap.report(iff.condition, "If condition must be a boolean")
        None
    }
  }

  def convertFor(fo: AST.For, ctx:Context): Option[IR.Statement] = ctx.symbols.lookupSymbol(fo.id) match {
    case None => assert(false, "Symbol not found."); None
    case Some(field) => field match {
      case f:FieldSymbol => {
        f.size match {
          case Some(s) => assert(false, "Cannot use an array as a for-loop variable"); return None
          case None =>
        }
        // Must loop over a variable isn't the best error message. params must be int?
        if (f.dtype != DTInt) {assert(false, "Must loop over a variable."); return None}
        val start:IR.Expr = convertExpr(fo.start, ctx)
        if (typeOfExpr(start) != DTInt) {assert(false, "Loop start value must be an int."); return None}
        val end:IR.Expr = convertExpr(fo.iter, ctx)
        if (typeOfExpr(end) != DTInt) {assert(false, "Loop end value must be an int."); return None}
        val block:IR.Block = convertBlock(fo.then, Context(ctx.symbols, true, ctx.returnType))
        return Some(IR.For(fo.id, start, end, block))
      }
      case _ => {assert(false, "Loop variable is not a field."); return None}
    }
  }

  def convertWhile(whil: AST.While, ctx:Context): Option[IR.While] = {
    // TODO(miles): report errors in blocks AFTER error in condition statement.
    val cond = convertExpr(whil.condition, ctx)
    val block = convertBlock(whil.block, Context(ctx.symbols, true, ctx.returnType))

    typeOfExpr(cond) match {
      case DTBool => Some(IR.While(cond, block, whil.max))
      case _ =>
        errors += srcmap.report(whil.condition, "While condition must be a boolean")
        None
    }
  }

  // TODO implement convertReturn
  def convertReturn (ret:AST.Return, ctx:Context): Option[IR.Statement] = {
    val optionExpr = convertExpr(ret.expr, ctx)
    optionExpr match {
      case Some(expr) => typeOfExpr(expr) match {
        case ctx.returnType => Some(IR.Return(optionExpr))
        case _ =>
          errors += srcmap.report(ret, "Return type must match the method signature")
          None
      }
      case None => ctx.returnType match {
        case DTVoid => Some(IR.Return(optionExpr))
        case _ =>
          errors += srcmap.report(ret, "Non-void method requires a return value")
          None
      }
    }
  }
}
