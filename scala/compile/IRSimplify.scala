package compile

object IRSimplifier {
  import IR._
  import IRShared._
  import SymbolTable._
  import TempVarGen._
  import FunctionalUtils._

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

    def flatten(tempGen: TempVarGen) : (List[Statement], Expr) = expr match {
      case UnaryOp(op, operand) => operand match {
        case _:Load => return (List(), UnaryOp(op, operand)) // Already simple!
        case e:Expr => {
          // Flatten the operand, and assign it to a temporary var
          val (operandStatements, operandExpr) = e.flatten(tempGen)
          val tempVar = tempGen.newVar(op.operandType())
          val statements = operandStatements :+ Assignment(Store(tempVar, None), operandExpr)
          val finalExpr = UnaryOp(op, LoadField(tempVar, None))
          return (statements, finalExpr)
        }
      }
      case BinOp(left, op, right) => (left, right) match {
        case (_:Load, _:Load) => return (List(), BinOp(left, op, right)) // Already simple!
        case (l:Expr, r:Load) => { // Left-operand is complex
          // Flatten the leftside
          val (leftStatements, finalLeftExpr) = l.flatten(tempGen)
          // Generate a temporary var for the left side of the expression
          val leftType = op match {
            case _:ArithmeticBinOp => DTInt
            case _:RelationalBinOp => DTInt
            case _:EqualityBinOp   => r.dtype()
            case _:BooleanBinOp    => DTBool
          }
          val leftTempVar = tempGen.newVar(leftType)
          val statements = leftStatements :+ Assignment(Store(leftTempVar, None), finalLeftExpr)
          val finalExpr = BinOp(LoadField(leftTempVar, None), op, r)
          return (statements, finalExpr)
        }
        case (l:Load, r:Expr) => { // Right-operand is complex
          // Flatten the right side
          val (rightStatements, finalRightExpr) = r.flatten(tempGen)
          // Generate a temporary var for the right side of the expression
          val rightType = op match {
            case _:ArithmeticBinOp => DTInt
            case _:RelationalBinOp => DTInt
            case _:EqualityBinOp   => l.dtype()
            case _:BooleanBinOp    => DTBool
          }
          val rightTempVar = tempGen.newVar(rightType)
          val statements = rightStatements :+ Assignment(Store(rightTempVar, None), finalRightExpr)
          val finalExpr = BinOp(l, op, LoadField(rightTempVar, None))
          return (statements, finalExpr)
        }
      }
    }
  }

  // Construct an AST from a parse tree
  // ptree - root program node
  // source - source code
  // Example Usage:
  //   val ast = ASTBuilder.parseProgram(parseTree).ast
  class IRSimplifier(var program: ProgramIR, filepath: String, code: String) {
  }
}
