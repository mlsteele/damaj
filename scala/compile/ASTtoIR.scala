package compile 
import SymbolTable._
// namespace collisions import AST._
// namespace collisions import IR._
//
// Code primarily borromed from ASTTools.scala.
// variable names come from the naming convention 
// established there
//
// to do: put callouts in symbol table
//Construct an IR from a AST
object IRBuilder{
  class IRConstructionException(msg: String) extends RuntimeException(msg)

  def convertProgram(ast: AST.ProgramAST): IR.ProgramIR = IR.ProgramIR( //what is the end of this line doing?
    //to do
   ast.callouts.map(convertCalloutDecl),
   ast.fields.map(convertFieldDecl),
   ast.methods.map(convertMethodDecl))
   def convertCalloutDecl(calloutDec: AST.CalloutDecl): IR.Callout = IR.Callout(calloutDec.id)
//     calloutList.map(AST.CalloutDecl => IR.CalloutDecl)
   def convertFieldDecl(fieldDec: AST.FieldDecl): IR.Field = IR.Field(fieldDec.dtype,field.id,field.size)
   def convertMethodDecl(meth: AST.MethodDecl): IR.Method {
     val method = IR.Method( meth.id,meth.args,meth.returns,meth.block,meth.params)
     symboltable.add(method)
     method
     /*  def convertMethodDeclAgr(meth: AST.MethodDeclArg): IR.Load = meth match{
     
   }*/
   //for convert assign assume know how to convert location to store  
  def convertAssignment(assign: AST.Assignment): IR.Assignment = IR.Assignment(locToStore(assign.left),convertExpr(assign.right))

  def convertExpr(expr: AST.Expr ): IR.Expr = IR.BoolLiteral(true)
  def convertID()
  def convertMethodDeclArg( )
  def convertDType( )
  def convertStatement(ast:AST.Statement): IR.Statement = ast match{
      case Assignment => convertAssignment(ast)
      case MethodCall => convertMethodCall(ast)
      case If => convertIf(ast)
      case For => convertFor(ast)
      case While => convertWhile(ast)
      case Return => convertReturn(ast)
      case Continue => convertContinue(ast)
     }
  

  def locToStore(loc: AST.Location): IR.Store{
    table = new SymbolTable
    val field:IR.Field = table.lookup(byID(loc.id))
    return IR.Store(field,loc.index


  }
  def convertBlock(block: AST.Block ):IR.Block = IR.Block(block.decls,block.stmts)
  def convertIf(iff: AST.If): IR.If = IR.If(iff.condition,iff.then,iff.elseb)
  def convertFor(fo: AST.For): IR.For = IR.For(fo.id,fo.start,fo.iter,fo.then)
  def convertWhile (whil:AST.While): IR.While = IR.While(whil.condition, whil.block,whil.max)
  def convertReturn (ret:AST.Return): IR.Return = IR.Return(ret.expr)
  //  def convertBreak ( ) this can be done through =>
  //  def convertContinue ( ) this can be done through =>
  def convertIntLiteral(int:AST.IntLiteral): IR.IntLiteral = IR.IntLiteral(int.value)
  def convertBoolLiteral(bool:AST.BoolLiteral):IR.BoolLiteral = IR.BoolLiteral(bool.value)
  def convertStrLiteral(string: AST.StrLiteral): IR.StrLiteral = IR.StrLiteral(string.value)
  def convertCharLiteral(char: AST.CharLiteral): IR.IntLiteral = IR.IntLiteral(char.value.toInt)
  //  convertMany[AST.CalloutDecl](ast,"callout_many",convertCalloutDecl)

}
