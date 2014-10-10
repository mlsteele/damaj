package compile

import IR._
import IR2._
import IRShared._
import SymbolTable._

// Construct an AST from a parse tree
// ptree - root program node
// source - source code
// Example Usage:
//   val ast = ASTBuilder.parseProgram(parseTree).ast
class IR2Builder(program: ProgramIR, filepath: String, code: String) {
  def convertProgram(ir: IR.ProgramIR): IR2.Program = {
    
    val fields = ir.symbols.symbols.flatMap(_ match {
      case c:CalloutSymbol => None
      case m:MethodSymbol => None
      case f:FieldSymbol => Some(convertField(f))
    })

    val methods = ir.symbols.symbols.flatMap(_ match {
      case c:CalloutSymbol => None
      case f:FieldSymbol => None
      case m:MethodSymbol => Some(convertMethod(m))
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

  def convertBlock(block: IR.Block): IR2.CFG = {
    // TODO
    IR2.CFG(IR2.Block(List()), IR2.Block(List()), new IdentityMap[IR2.Block, Transition]())
  }
}
