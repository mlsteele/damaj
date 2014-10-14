package compile

import IR._
import IR2._
import SymbolTable._

// Construct an IR2 from an IR
// Example Usage:
//   val ir2 = new IR2Builder(ir1).ir2
class IR2Builder(program: ProgramIR) {

  val ir2 = convertProgram(program)

  // Programming error.
  class IR2ConstructionException(msg: String) extends RuntimeException(msg)

  def convertProgram(ir: IR.ProgramIR): IR2.Program = {
    val fields = ir.symbols.symbols.flatMap(_ match {
      case f:FieldSymbol => Some(convertField(f))
      case _ => None
    })

    val mainMethod = ir.symbols.symbols
      .filter(_.id == "main") match {
        case List(main: MethodSymbol) => convertMethod(main)
        case _ => throw new IR2ConstructionException("Program must have one main method.")
      }

    val methods = ir.symbols.symbols.flatMap(_ match {
      case m:MethodSymbol => Some(convertMethod(m))
      case _ => None
    })

    IR2.Program(fields, mainMethod, methods)
  }

  def convertField(field: FieldSymbol): IR2.Field = Field(field.id, field.size)
  
  def convertMethod(method: MethodSymbol): IR2.Method = IR2.Method(
    method.id,
    method.params.symbols.flatMap( _ match {
      case f:FieldSymbol => Some(convertField(f))
      case _ => None
    }),
    convertBlock(method.block))

  def convertBlock(block: IR.Block): CFG =
    block.stmts.map(CFGFactory.fromStatement).fold(CFGFactory.dummy)(_ ++ _)
 
  def convertStatement(statement: IR.Statement): CFG = statement match {
      case IR.If(pre, condition, thenb, elseb) =>
        val startCFG = pre.map(convertStatement).reduceLeft((x,y) => x ++ y)
        val thenCFG = convertBlock(thenb)
        val endBlock = CFGFactory.nopBlock
        val edges = startCFG.edges ++ thenCFG.edges
        edges.put(thenCFG.end, Edge(endBlock))

        elseb match {
          case Some(b) =>
            val elseCFG = convertBlock(b)
            edges.putAll(elseCFG.edges)
            edges.put(startCFG.end, Fork(condition, thenCFG.start, elseCFG.start))
            edges.put(elseCFG.end, Edge(endBlock))
          case None =>
            edges.put(startCFG.end, Fork(condition, thenCFG.start, endBlock))
        }
        new CFG(startCFG.start, endBlock, edges)

      case forb:IR.For => 
        assert(false, "For was not preprocessed away!")
        CFGFactory.dummy
      case IR.While(pre, condition, block, max) =>
        assert(max == None, "While didn't preprocess out max!")
        val startCFG = pre.map(convertStatement).reduceLeft((x,y) => x ++ y)
        val blockCFG = convertBlock(block)
        val endBlock = CFGFactory.nopBlock
        val edges = startCFG.edges ++ blockCFG.edges
        edges.put(startCFG.end, Fork(condition, blockCFG.start, endBlock))
        edges.put(blockCFG.end, Edge(startCFG.start))

        new CFG(startCFG.start, endBlock, edges)
      case _ => CFGFactory.fromStatement(statement)
  }
}
