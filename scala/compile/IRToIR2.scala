package compile

import IR._
import IR2._
import SymbolTable._
import IRShared._

// Construct an IR2 from an IR
// Turns control flow directives into CFG structures.
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

    val methods = ir.symbols.symbols
      .filter(_.id != "main")
      .flatMap(_ match {
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
    convertBlock(method.block),
  method.returns)

  def convertBlock(block: IR.Block): CFG = {
    block.stmts
      .map(stmt => convertStatement(stmt, block.fields))
      .fold(CFGFactory.dummy)(_ ++ _)
  }
 
  def convertStatement(statement: IR.Statement, symbols: SymbolTable): CFG = statement match {
      case IR.If(pre, condition, thenb, elseb) =>
        val startCFG = pre.map(x => convertStatement(x, symbols)).reduceLeft((x,y) => x ++ y)
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
        val startCFG = pre.map(x => convertStatement(x, symbols)).reduceLeft((x,y) => x ++ y)
        val blockCFG = convertBlock(block)
        val endBlock = CFGFactory.nopBlock
        val edges = startCFG.edges ++ blockCFG.edges
        edges.put(startCFG.end, Fork(condition, blockCFG.start, endBlock))
        edges.put(blockCFG.end, Edge(startCFG.start))

        new CFG(startCFG.start, endBlock, edges)
      case IR.MethodCall(s, args) =>
        CFGFactory.fromStatement(IR2.Call(s.id, args.map(convertArg)), symbols)
      case IR.CalloutCall(s, args)=> 
        CFGFactory.fromStatement(IR2.Call(s.id, args.map(convertArg)), symbols)
      case IR.Assignment(left, right) =>
        CFGFactory.fromStatement(
          IR2.Assignment(convertStore(left), convertExpr(right)), symbols)
      case _:IR.Break => throw new RuntimeException("Not yet implemented") // TODO
      case _:IR.Continue => throw new RuntimeException("Not yet implemented") // TODO
      case _:IR.Return => throw new RuntimeException("Not yet implemented") // TODO
  }

  def convertArg(arg: Either[StrLiteral, IR.Expr]): Either[StrLiteral, IR2.Expr] = arg match {
    case Left(s) => Left(s)
    case Right(s) => Right(convertExpr(s))
  }

  def convertStore(store: IR.Store): IR2.Store = 
    IR2.Store(store.to, store.index.map(exprToLoad))

  def convertOptionExpr(oexpr: Option[IR.Expr]) = oexpr match {
    case Some(e) => Some(convertExpr(e))
    case None => None
  }

  def convertExpr(expr: IR.Expr):IR2.Expr = expr match {
    case IR.BinOp(l, op, r) => IR2.BinOp(exprToLoad(l), op, exprToLoad(r))
    case IR.UnaryOp(op, r) => IR2.UnaryOp(op, exprToLoad(r))
    case IR.Ternary(condition, l, r) =>
      IR2.Ternary(convertExpr(condition), exprToLoad(l), exprToLoad(r))
    case l:IR.LoadField => exprToLoad(expr)
    case l:IR.LoadInt => exprToLoad(expr)
    case l:IR.LoadBool => exprToLoad(expr)
    case m:IR.MethodCall => IR2.Call(m.method.id, m.args.map(convertArg))
    case c:IR.CalloutCall => IR2.Call(c.callout.id, c.args.map(convertArg))
  }

  // IR preprocessing guarantees that some Exprs are only Loads.
  // This asserts and casts an Expr to a Load.
  def exprToLoad(expr: IR.Expr): IR2.Load = expr match {
    case IR.LoadField(from, index) => IR2.LoadField(from, index.map(exprToLoad))
    case IR.LoadInt(value) => IR2.LoadLiteral(value)
    case IR.LoadBool(value) => value match {
      case true => IR2.LoadLiteral(1)
      case false => IR2.LoadLiteral(0)
    }
    case _ => throw new IR2ConstructionException("That expr should be a load!")
  }
}
