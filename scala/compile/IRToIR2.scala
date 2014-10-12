package compile

import IR._
import IR2._
import IRShared._
import SymbolTable._
import TempVarGen._
// Construct an AST from a parse tree
// ptree - root program node
// source - source code
// Example Usage:
//   val ast = ASTBuilder.parseProgram(parseTree).ast
class IR2Builder(program: ProgramIR, filepath: String, code: String) {
  def convertProgram(ir: IR.ProgramIR): IR2.Program = {
    
    val fields = ir.symbols.symbols.flatMap(_ match {
      case f:FieldSymbol => Some(convertField(f))
      case _ => None
    })

    val methods = ir.symbols.symbols.flatMap(_ match {
      case m:MethodSymbol => Some(convertMethod(m))
      case _ => None
    })

    IR2.Program(fields, methods)
  }

  def convertField(field: FieldSymbol): IR2.Field = Field(field.id, field.size)
  
  def convertMethod(method: MethodSymbol): IR2.Method = IR2.Method(
    method.id,
    method.params.symbols.flatMap( _ match {
      case f:FieldSymbol => Some(convertField(f))
      case _ => None
    }),
    convertBlock(method.block))

  def convertBlock(block: IR.Block): CFG = {
    // TODO
    // For each statement convert it to a CFG, then combine them
    // with some soon-to-be-defined method on CFGs
    new CFG(IR2.Block(List()), IR2.Block(List()), new IdentityMap[IR2.Block, Transition]())
  }

  val dummyCFG = new CFG(IR2.Block(List()), IR2.Block(List()), new IdentityMap[IR2.Block, Transition]())
  def convertStatement(statement: IR.Statement): CFG = statement match {
    // TODO
    // Most statements will be just copied verbatim into the new block
    // If's, while's, and for's are the interesting parts
    // Don't forget to convert expressions
    case ifb:IR.If => dummyCFG //TODO
    case forb:IR.For => dummyCFG //TODO
    case whileb:IR.While => dummyCFG //TODO
    case _ =>
      val b = IR2.Block(List(statement))
      new CFG(b, b, new IR2.edgeMap())
  }


}
