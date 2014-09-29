package compile 
import SymbolTable._
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
  class IRConstructionException(msg: String) extends RuntimeException(msg)

  def convertProgram(ast: AST.ProgramAST): IR.ProgramIR = IR.ProgramIR( //what is the end of this line doing?
    // TODO
    ast.callouts.map(convertCalloutDecl),
    ast.fields.map(convertFieldDecl),
    ast.methods.map(convertMethodDecl))
    def convertCalloutDecl(calloutDec: AST.CalloutDecl): IR.Callout = IR.Callout(calloutDec.id)
    // calloutList.map(AST.CalloutDecl => IR.CalloutDecl)
    def convertFieldDecl(fieldDec: AST.FieldDecl): IR.Field = IR.Field(fieldDec.dtype,field.id,field.size)
    def convertMethodDecl(meth: AST.MethodDecl): IR.Method = {
    var method:IR.Method = IR.Method( meth.id,meth.args,meth.returns,meth.block,meth.params)
    var table = new SymbolTable()
    table.add(method)
    method
  }
     
  def convertAssignment(assign: AST.Assignment): IR.Assignment = IR.Assignment(locToStore(assign.left),convertExpr(assign.right))

  def convertExpr(expr: AST.Expr ): IR.Expr = IR.BoolLiteral(true)

  // TODO
  // def convertID()
  // def convertMethodDeclArg( )
  // def convertDType( )

  def convertStatement(ast:AST.Statement): IR.Statement = ast match{
    case a:AST.Assignment => convertAssignment(a)
    case a:AST.MethodCall => convertMethodCall(a)
    case a:AST.If => convertIf(a)
    case a:AST.For => convertFor(a)
    case a:AST.While => convertWhile(a)
    case a:AST.Return => convertReturn(a)
    case AST.Continue => IR.Continue
  }

  def locToStore(loc: AST.Location): IR.Store= {
    val table = new SymbolTable
    val field:IR.Field = table.lookup(byID(loc.id))
    // IR.Store(field,loc.index)
  }

  def convertBlock(block: AST.Block ):IR.Block = IR.Block(block.decls,block.stmts)
  def convertIf(iff: AST.If): IR.If = IR.If(convertExpr(iff.condition),convertBlock(iff.then),iff.elseb.map(convertBlock))
  def convertFor(fo: AST.For): IR.For = IR.For(fo.id,convertExpr(fo.start),convertExpr(fo.iter),convertBlock(fo.then))
  def convertWhile (whil:AST.While): IR.While = IR.While(convertExpr(whil.condition),convertBlock(whil.block),whil.max)
  def convertReturn (ret:AST.Return): IR.Return = IR.Return(ret.expr.map(convertExpr))
  //  def convertBreak ( ) this can be done through =>
  //  def convertContinue ( ) this can be done through =>
  def convertIntLiteral(int:AST.IntLiteral): IR.IntLiteral = IR.IntLiteral(int.value)
  def convertBoolLiteral(bool:AST.BoolLiteral):IR.BoolLiteral = IR.BoolLiteral(bool.value)
  def convertStrLiteral(string: AST.StrLiteral): IR.StrLiteral = IR.StrLiteral(string.value)
  def convertCharLiteral(char: AST.CharLiteral): IR.IntLiteral = IR.IntLiteral(char.value.toInt)
  //  convertMany[AST.CalloutDecl](ast,"callout_many",convertCalloutDecl)
}
