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

    IR.ProgramIR(symbols)
  }

  def convertFieldDecl(ast: AST.FieldDecl, symbols:SymbolTable): FieldSymbol = FieldSymbol(ast.dtype, ast.id, ast.size)

  def convertMethodDecl(meth: AST.MethodDecl, parent: SymbolTable): MethodSymbol = {
    val paramsTable = new SymbolTable(parent)
    val duplicate_args = paramsTable.addSymbols(meth.args.map(convertMethodDeclArg(_, parent)))
    assert(duplicate_args.length == 0, "TODO error reporting" + duplicate_args)
    return MethodSymbol(meth.id, paramsTable, meth.returns, convertBlock(meth.block, paramsTable))
  }

  // Only used after crashing because of an error
  val dummyExpr = IR.LoadLiteral(BoolLiteral(false))
     
  def convertExpr(expr: AST.Expr, symbols:SymbolTable): IR.Expr = expr match {
    case AST.BinOp(left, op, right) => IR.BinOp(convertExpr(left, symbols), op, convertExpr(right, symbols))
    case AST.UnaryOp(op, right) => IR.UnaryOp(op, convertExpr(right, symbols))
    case AST.Ternary(condition, left, right) => 
      IR.Ternary(convertExpr(condition, symbols), convertExpr(left, symbols), convertExpr(right, symbols))
    case AST.Literal(l) => IR.LoadLiteral(l)
    // TODO(jessk): pass down `symbols`
    case AST.Location(id, index) => 
      val fs = symbols.lookupSymbol(id)
      fs match {
        case Some(s) => s match {
          case f:FieldSymbol => IR.LoadField(f, convertExpr(index, symbols))
          case _ =>
            assert(false, "Location must be a field")
            dummyExpr
        }
        case _ =>
          assert(false, "Unknown identifier")
          dummyExpr
      }
    case a:AST.MethodCall => convertMethodCall(a, symbols) match {
      case Some(x) => x
      case None => assert(false, "Unknown identifier"); dummyExpr
    }
  }

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

  //def verifyExpr(expr: IR.Expr): IR.Expr = expr match {
    // TODO
  //}

  // TODO
  // def convertID()
  def convertMethodDeclArg(ast: AST.MethodDeclArg, symbols:SymbolTable): FieldSymbol = FieldSymbol(ast.dtype, ast.id, None)

  val dummyStatement = IR.Continue
  def convertStatement(ast:AST.Statement, symbols:SymbolTable): IR.Statement = ast match{
    // TODO uncomment/comment these and uncomment convertAssignment when you're ready to work on it.
    // case a:AST.Assignment => convertAssignment(a)
    case a:AST.Assignment => IR.Break
    case a:AST.MethodCall => convertMethodCall(a, symbols) match {
      case Some(x) => x
      case None => assert(false, "Method does not exist " + a.id)
      dummyStatement
    }
    case a:AST.If => convertIf(a, symbols)
    case a:AST.For => convertFor(a, symbols)
    case a:AST.While => convertWhile(a, symbols)
    case a:AST.Return => convertReturn(a, symbols)
    case AST.Break => IR.Break
    case AST.Continue => IR.Continue
  }

  def convertMethodCall(ast: AST.MethodCall, symbols:SymbolTable): Option[IR.Call] = {
    val symbol = symbols.lookupSymbol(ast.id)
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

  // def convertAssignment(assign: AST.Assignment): IR.Assignment = IR.Assignment(locToStore(assign.left), convertExpr(assign.right))

  def locToStore(loc: AST.Location,symbols:SymbolTable): IR.Store = {
    // // TODO
    val table = new SymbolTable()
    val field = table.lookupSymbol(loc.id)
    field match{
      case Some(f)=> f match{
        case f:FieldSymbol => IR.Store(f, convertExpr(loc.index,symbols))
        case _ => IR.Store(FieldSymbol(DTVoid,"void",None),Some(dummyExpr))// TODO (Andres): replace this dummy error with an actual one
      }
        case None =>IR.Store(FieldSymbol(DTVoid,"void",None),Some(dummyExpr)) //TODO (Andres:) replace this dummy error with an actual one
   
    }
  }
  def convertBlock(block: AST.Block, symbols:SymbolTable): IR.Block = {
    val localtable = new SymbolTable(Some(symbols))
    // TODO enter symbols into table
    IR.Block(block.stmts.map(convertStatement(_, localtable)), localtable)
  }

  // TODO these just return dummies for now. Comment/Uncomment when you start working on these.
  // def convertIf(iff: AST.If): IR.If = IR.If(convertExpr(iff.condition), convertBlock(iff.then), iff.elseb.map(convertBlock))
  // def convertFor(fo: AST.For): IR.For = IR.For(fo.id, convertExpr(fo.start), convertExpr(fo.iter), convertBlock(fo.then))
  // def convertWhile (whil:AST.While): IR.While = IR.While(convertExpr(whil.condition), convertBlock(whil.block), whil.max)
  // def convertReturn (ret:AST.Return): IR.Return = IR.Return(ret.expr.map(convertExpr))
  def convertIf(iff: AST.If, symbols:SymbolTable): IR.Statement = IR.Break
  def convertFor(fo: AST.For, symbols:SymbolTable): IR.Statement = IR.Break
  def convertWhile (whil:AST.While, symbols:SymbolTable): IR.Statement = IR.Break
  def convertReturn (ret:AST.Return, symbols:SymbolTable): IR.Statement = IR.Break

  //  def convertBreak ( ) this can be done through =>
  //  def convertContinue ( ) this can be done through =>
  //  convertMany[AST.CalloutDecl](ast, "callout_many", convertCalloutDecl)
}
