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

    /**
      * Takes an expression, and generates a list of statements and
      * temporary variable assignments that are needed to generate the
      * expression, if the expression is not simple.  For example,
      * (z = a + b + c + d) will be translated into the following statements
      * and final expr:
      * statements = [t1 = a + b, t2 = t1 + c]
      * final expr: (t2 + d)
      */
    def flatten(tempGen: TempVarGen) : (List[Statement], Expr) = expr match {
      case load:Load => return (List(), load) // Already simple!
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
        case _ => {
          // Assume both sides are complex. Optimizer will clean this up
          // Flatten the leftside and generate a temporary var for the left side of the expression
          val (leftStatements, finalLeftExpr) = left.flatten(tempGen)
          val leftTempVar = tempGen.newVar(typeOfExpr(left))

          // Flatten the right and generate a temporary var for the right side of the expression
          val (rightStatements, finalRightExpr) = right.flatten(tempGen)
          val rightTempVar = tempGen.newVar(typeOfExpr(right))

          // Combine statements for generating left and right sides
          val statements = leftStatements :+
            Assignment(Store(leftTempVar, None), finalLeftExpr) :+ 
            Assignment(Store(rightTempVar, None), finalRightExpr)

          // Final expr is (tempLeft 'op' tempRight)
          val finalExpr = BinOp(LoadField(leftTempVar, None), op, LoadField(rightTempVar, None))
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
