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

  def convertProgram(ast: AST.ProgramAST): IR.ProgramIR = IR.ProgramIR( //what is the end of this line doing?
    // TODO
    // ast.callouts.map(convertCalloutDecl),
    // ast.fields.map(convertFieldDecl),
    // ast.methods.map(convertMethodDecl))
    new SymbolTable(),
    new SymbolTable(),
    new SymbolTable())

  def convertCalloutDecl(calloutDec: AST.CalloutDecl): IR.Callout = IR.Callout(calloutDec.id)
  // calloutList.map(AST.CalloutDecl => IR.CalloutDecl)

  def convertFieldDecl(ast: AST.FieldDecl): IR.Field = IR.Field(ast.dtype, ast.id, ast.size)

  def convertMethodDecl(meth: AST.MethodDecl): IR.Method = {
    // TODO fill table
    val params = new SymbolTable()
    var method:IR.Method = IR.Method(meth.id, params, meth.returns, convertBlock(meth.block))
    method
  }
     
  // TODO
  def convertExpr(expr: AST.Expr): IR.Expr = IR.Literal(BoolLiteral(true))

  // TODO
  // def convertID()
  // def convertMethodDeclArg( )
  // def convertDType( )

  def convertStatement(ast:AST.Statement): IR.Statement = ast match{
    // TODO uncomment/comment these and uncomment convertAssignment when you're ready to work on it.
    // case a:AST.Assignment => convertAssignment(a)
    case a:AST.Assignment => IR.Break
    case a:AST.MethodCall => convertMethodCall(a)
    case a:AST.If => convertIf(a)
    case a:AST.For => convertFor(a)
    case a:AST.While => convertWhile(a)
    case a:AST.Return => convertReturn(a)
    case AST.Break => IR.Break
    case AST.Continue => IR.Continue
  }

  def convertMethodCall(ast: AST.MethodCall) = IR.MethodCall(ast.id, ast.args.map(convertMethodCallArg))

  def convertMethodCallArg(ast: Either[StrLiteral, AST.Expr]): Either[StrLiteral, IR.Expr] = ast match {
    case Left(x) => Left(x)
    case Right(x) => Right(convertExpr(x))
  }

  // def convertAssignment(assign: AST.Assignment): IR.Assignment = IR.Assignment(locToStore(assign.left), convertExpr(assign.right))

  // def locToStore(loc: AST.Location): IR.Store = {
    // // TODO
    // val table = new SymbolTable()
    // val field:IR.Field = table.lookup(byID(loc.id))
    // // IR.Store(field, loc.index)
  // }

  def convertBlock(block: AST.Block ): IR.Block = {
    val localtable = new SymbolTable()
    // TODO enter symbols into table
    IR.Block(block.stmts.map(convertStatement), localtable)
  }

  // TODO these just return dummies for now. Comment/Uncomment when you start working on these.
  // def convertIf(iff: AST.If): IR.If = IR.If(convertExpr(iff.condition), convertBlock(iff.then), iff.elseb.map(convertBlock))
  // def convertFor(fo: AST.For): IR.For = IR.For(fo.id, convertExpr(fo.start), convertExpr(fo.iter), convertBlock(fo.then))
  // def convertWhile (whil:AST.While): IR.While = IR.While(convertExpr(whil.condition), convertBlock(whil.block), whil.max)
  // def convertReturn (ret:AST.Return): IR.Return = IR.Return(ret.expr.map(convertExpr))
  def convertIf(iff: AST.If): IR.Statement = IR.Break
  def convertFor(fo: AST.For): IR.Statement = IR.Break
  def convertWhile (whil:AST.While): IR.Statement = IR.Break
  def convertReturn (ret:AST.Return): IR.Statement = IR.Break

  //  def convertBreak ( ) this can be done through =>
  //  def convertContinue ( ) this can be done through =>
  //  convertMany[AST.CalloutDecl](ast, "callout_many", convertCalloutDecl)
}
