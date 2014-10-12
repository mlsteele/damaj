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
          
          // returns (statements, finalExpr)
          op match {
            case a:And =>
              val finalExprValue = tempGen.newVar(DTBool)
              (List(
                If(leftStatements :+ Assignment(Store(leftTempVar, None), finalLeftExpr),
                  LoadField(leftTempVar, None),
                  Block(List(
                    If(rightStatements :+ Assignment(Store(rightTempVar, None), finalRightExpr),
                      LoadField(rightTempVar, None),
                      Block(List(Assignment(Store(finalExprValue, None), LoadBool(true))), new SymbolTable()),
                      Some(Block(List(Assignment(Store(finalExprValue, None), LoadBool(false))), new SymbolTable())))), new SymbolTable()),
                  Some(Block(List(Assignment(Store(finalExprValue, None), LoadBool(false))), new SymbolTable())))),
              // finalExpr:
              LoadField(finalExprValue, None))
            case o:Or =>
              val finalExprValue = tempGen.newVar(DTBool)
              (List(
                If(leftStatements :+ Assignment(Store(leftTempVar, None), finalLeftExpr),
                  UnaryOp(Not(),LoadField(leftTempVar, None)),
                  Block(List(
                    If(rightStatements :+ Assignment(Store(rightTempVar, None), finalRightExpr),
                      UnaryOp(Not(),LoadField(rightTempVar, None)),
                      Block(List(Assignment(Store(finalExprValue, None), LoadBool(false))), new SymbolTable()),
                      Some(Block(List(Assignment(Store(finalExprValue, None), LoadBool(true))), new SymbolTable())))), new SymbolTable()),
                  Some(Block(List(Assignment(Store(finalExprValue, None), LoadBool(true))), new SymbolTable())))),
              // finalExpr:
              LoadField(finalExprValue, None))
            case _ =>

              // Combine statements for generating left and right sides
              ((leftStatements ++ rightStatements) :+
                Assignment(Store(leftTempVar, None), finalLeftExpr) :+ 
                Assignment(Store(rightTempVar, None), finalRightExpr),
              // finalExpr:
              BinOp(LoadField(leftTempVar, None), op, LoadField(rightTempVar, None)))
          }
        }
      }

      case Ternary(cond, left, right) => (cond, left, right) match {
        case (_:Load, _:Load, _:Load) => return (List(), expr) //  Already simple!
        case _ => {
          val (condStatements, finalCondExpr) = cond.flatten(tempGen)
          val condTempVar = tempGen.newVar(typeOfExpr(finalCondExpr))

          val (leftStatements, finalLeftExpr) = left.flatten(tempGen)
          val leftTempVar = tempGen.newVar(typeOfExpr(finalLeftExpr))

          val (rightStatements, finalRightExpr) = right.flatten(tempGen)
          val rightTempVar = tempGen.newVar(typeOfExpr(finalRightExpr))

          val statements = (condStatements ++ leftStatements ++ rightStatements) :+
            Assignment(Store(condTempVar, None), finalCondExpr) :+
            Assignment(Store(leftTempVar, None), finalLeftExpr) :+
            Assignment(Store(rightTempVar, None), finalRightExpr)

          val finalExpr = Ternary(LoadField(condTempVar, None), LoadField(leftTempVar, None), LoadField(rightTempVar, None))
          return (statements, finalExpr)
        }
      }

      case MethodCall(symbol, args) => {
        var statements: List[Statement] = List()
        var tempVars: List[Expr] = List()
        for (eitherArg <- args) {
          // Extract the Rights out of the args, because why the hell can a method take a StringLiteral
          eitherArg.map (arg => {
            // flatten all the args, collecting their dependency statements and temporary vars
            val (argStatements, argExpr) = arg.flatten(tempGen)
            val argTempVar = tempGen.newVarLike(argExpr)
            statements = (statements ++ argStatements) :+
              Assignment(Store(argTempVar, None), argExpr)
            tempVars = tempVars :+ LoadField(argTempVar, None)
          })
        }
        // Make a new method call using the temporary vars as the args
        val finalExpr = MethodCall(symbol, tempVars.map(Right(_)))
        return (statements, finalExpr)
      }

      case CalloutCall(symbol, args) => {
        var statements: List[Statement] = List()
        var tempVars: List[Either[StrLiteral, Expr]] = List()
        for (eitherArg <- args) {
          eitherArg match {
            case Right(arg) => {
              // flatten all the args, collecting their dependency statements and temporary vars
              val (argStatements, argExpr) = arg.flatten(tempGen)
              val argTempVar = tempGen.newVarLike(argExpr)
              statements = (statements ++ argStatements) :+
                Assignment(Store(argTempVar, None), argExpr)
              tempVars = tempVars :+ Right(LoadField(argTempVar, None))
            }
            case Left(str) => {
              tempVars = tempVars :+ Left(str)
            }
          }
        }
        // Make a new callout call using the temporary vars as the args
        val finalExpr = CalloutCall(symbol, tempVars)
        return (statements, finalExpr)
      }
    }
  }

  implicit class MaybeSimpleStatement(stmt: Statement) {
    /**
      * Determines whether a statement is simple.
      * This means that any arguments or operands to a statement
      * are only Loads.
      */
    def isSimple() : Boolean = stmt match {
      case Assignment(left, right) => right.isSimple()
      case m:MethodCall => {val e:Expr = m; e.isSimple()}
      case c:CalloutCall => {val e:Expr = c; e.isSimple()}
      case If(preStmts, condition, thenb, elseb) => {
        var preds: List[Boolean] = preStmts.map(_.isSimple()) ++
          thenb.stmts.map(_.isSimple())
        elseb.map(_.stmts.map(s => preds = preds :+ s.isSimple()))
        preds = preds :+ condition.isSimple()
        return all(preds)
      }
      case For(id, startPreStmts, start, iterPreStmts, iter, thenb) => {
        val preds: List[Boolean] = startPreStmts.map(_.isSimple()) ++
          iterPreStmts.map(_.isSimple()) ++
          thenb.stmts.map(_.isSimple()) :+
          start.isSimple() :+
          iter.isSimple()
        return all(preds)
      }
      case While(preStmts, condition, block, _) => {
        val preds: List[Boolean] = preStmts.map(_.isSimple()) ++
          block.stmts.map(_.isSimple()) :+
          condition.isSimple()
        return all(preds)
      }
      case Return(None) => true
      case Return(Some(e)) => e.isSimple()
      case Break => true
      case Continue => true
    }

    /* Converts a statement (possibly simple or non-simple) into a list of
     * equivalent statements that are all simple by generating
     * temporary variables for complex sub-expressions.
     */
    def flatten(tempGen: TempVarGen) : List[Statement] = stmt match {
      case Assignment(left, right) => {
        val (stmts, finalExpr) = right.flatten(tempGen)
        return stmts :+ Assignment(left, finalExpr)
      }
      // MethodCall and CalloutCall have weird casts because they
      // are both Exprs and Statements
      case m:MethodCall => {
        val (stmts, finalExpr) = m.asInstanceOf[Expr].flatten(tempGen)
        return stmts :+ finalExpr.asInstanceOf[MethodCall]
      }
      case c:CalloutCall => {
        val (stmts, finalExpr) = c.asInstanceOf[Expr].flatten(tempGen)
        return stmts :+ finalExpr.asInstanceOf[CalloutCall]
      }
      case If(preStmts, cond, thenb, elseb) => {
        val (condStmts, condExpr) = cond.flatten(tempGen)
        val newPreStmts = preStmts.flatMap(_.flatten(tempGen)) ++
          condStmts
        return List(If(newPreStmts,
          condExpr,
          thenb.flatten(),
          elseb.map(_.flatten())
        ))
      }
    }
  }

  implicit class MaybeSimpleBlock (var block: Block) {
    def flatten() : Block = {
      val tempGen = new TempVarGen(block.fields)
      val newStmts = block.stmts.flatMap(_.flatten(tempGen))
      return Block(newStmts, block.fields)
    }
  }

  // Construct an AST from a parse tree
  // ptree - root program node
  // source - source code
  // Example Usage:
  //   val ast = ASTBuilder.parseProgram(parseTree).ast
  class IRSimplifier(var program: ProgramIR, filepath: String, code: String) {
    def simplify() : ProgramIR = {
      val methods = program.symbols.symbols.filter(_.isMethod())
      return program
    }

    private def simplifyMethod(method: MethodSymbol) : MethodSymbol = {
      return method
    }
  }
}
