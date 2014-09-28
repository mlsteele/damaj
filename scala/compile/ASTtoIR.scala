package compile 
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
   def convertMethodDecl(meth: AST.MethodDecl): IR.Method = IR.Method( meth.id,meth.args,meth.returns,meth.block,meth.params)
  
   //for convert assign assume know how to convert location to store  
  def convertAssignment(assign: AST.Assignment): IR.Assignment = IR.Assignment(locToStore(assign.left),convertExpr(assign.right))
/*
  def convertExpr(  )
  def convertID(  )
  def convertMethodDeclArg( )
  def convertDType( )
  def convertBlock( )
  def convertStatement( )
  def locToStore( )
  def convertBlock( )
  def convertStrLiteral(  )
  def convertIf(  )
  def convertFor( )
  def convertWhile ( )
  def convertReturn ( )
  def convertBreak ( )
  def convertContinue ( )
   //  convertMany[AST.CalloutDecl](ast,"callout_many",convertCalloutDecl)
*/
}



