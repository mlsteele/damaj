package compile

import IR._
import IRShared._
import SymbolTable._
import TempVarGen._

object IRToCFG {
  def all(conds: List[Boolean]) : Boolean = conds.filter(_==false).length == 0

  implicit class EnhancedLoad(load: Load) {
    // Gets the type of a load
    def dtype(): DType = load match {
      case LoadField(from, _) => from.dtype
      case LoadInt(_)         => DTInt
      case LoadBool(_)        => DTBool
    }
  }

  implicit class MaybeSimpleExpr (expr: Expr) {
    def isLoad() : Boolean = expr match {
      case l:Load => true
      case _      => false
    }

    // Determines whether an expression is "simple"
    // 
    def isSimple() : Boolean = expr match {
      case BinOp(left, _, right) => left.isLoad() && right.isLoad()
      case UnaryOp(_, right)     => right.isLoad()
      case Ternary(cond, left, right) => cond.isLoad() && left.isLoad() && right.isLoad()
      case c:CalloutCall => all(c.args.map {
        case Left(_) => true
        case Right(e) => e.isSimple()
      })
      case m:MethodCall => all(m.args.map {
        case Left(_) => true
        case Right(e) => e.isSimple()
      })
      case l:Load => true
    }
  }

  // Construct an AST from a parse tree
  // ptree - root program node
  // source - source code
  // Example Usage:
  //   val ast = ASTBuilder.parseProgram(parseTree).ast
  class CFGBuilder(var program: ProgramIR, filepath: String, code: String) {
  }
}
