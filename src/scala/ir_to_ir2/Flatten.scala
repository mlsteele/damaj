package compile

object Flatten {
  import IR._
  import IRShared._
  import SymbolTable._
  import TempVarGen._
  import FunctionalUtils._
  import IRPrinter._
  /*
   * Flattens DECAF statements so that all operators and function
   * calls operate only on loads, such that there are no nested
   * expressions.
   * This method mutates the input program, in addition to returning a new one.
   */
  def flatten(program: ProgramIR): ProgramIR = {
    val methods: List[MethodSymbol] = program.symbols.symbols.filter(_.isMethod()).asInstanceOf[List[MethodSymbol]]
    methods.map(m => m.block = m.block.flatten())
    for (m <- methods) {
      assert(m.block.isSimple(),
        "Method %s was not correctly simplified!\n%s".format(m.id, printMethod(m)))
      }
    return program
  }

  implicit class EnhancedLoad(load: Load) {
    // Gets the type of a load
    def dtype(): DType = load match {
      case LoadField(from, _) => from.dtype
      case LoadInt(_)         => DTInt
      case LoadBool(_)        => DTBool
    }
  }

  implicit class MaybeSimpleExpr (expr: Expr) {
    def isSimpleLoad() : Boolean = expr match {
      case l:Load => l match {
        case LoadField(_, None) => true
        case LoadField(_, Some(_)) => false
        case _:LoadInt   => true
        case _:LoadBool  => true
      }
      case _ => false
    }

    // Determines whether an expression is "simple"
    // 
    def isSimple() : Boolean = expr match {
      case BinOp(left, _, right) => left.isSimpleLoad() && right.isSimpleLoad()
      case UnaryOp(_, right)     => right.isSimpleLoad()
      case Ternary(cond, left, right) => cond.isSimpleLoad() && left.isSimpleLoad() && right.isSimpleLoad()
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
      case LoadField(from, Some(index)) => {
        // Flatten the index
        val (indexStmts, finalIndexExpr) = index.flatten(tempGen)
        val indexTempVar = tempGen.newIntVar()
        return (
          indexStmts :+ Assignment(Store(indexTempVar, None), finalIndexExpr),
          LoadField(from, Some(LoadField(indexTempVar, None)))
        )
      }

      case l:Load => (List(),l)

      case UnaryOp(op, e) => {
        // Flatten the operand, and assign it to a temporary var
        val (operandStatements, operandExpr) = e.flatten(tempGen)
        val tempVar = tempGen.newVar(op.operandType())
        val statements = operandStatements :+ Assignment(Store(tempVar, None), operandExpr)
        val finalExpr = UnaryOp(op, LoadField(tempVar, None))
        return (statements, finalExpr)
      }

      case BinOp(left, op, right) => {
        // Assume both sides are complex. Optimizer will clean this up
        // Flatten the leftside and generate a temporary var for the left side of the expression
        val (leftStatements, finalLeftExpr) = left.flatten(tempGen)
        val leftTempVar = tempGen.newVar(typeOfExpr(left))

        // Flatten the right and generate a temporary var for the right side of the expression
        val (rightStatements, finalRightExpr) = right.flatten(tempGen)
        val rightTempVar = tempGen.newVar(typeOfExpr(right))

        val statements = (leftStatements ++ rightStatements) :+
          Assignment(Store(leftTempVar, None), finalLeftExpr) :+
          Assignment(Store(rightTempVar, None), finalRightExpr)

        return (
          statements,
          BinOp(LoadField(leftTempVar, None), op, LoadField(rightTempVar, None))
        )
      }

      case Ternary(cond, left, right) => {
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
      case Assignment(Store(_, None), right) => right.isSimple() // Simple scalar access
      case Assignment(Store(_, Some(index)), right) => index.isSimpleLoad() && right.isSimpleLoad()

      case m:MethodCall => {val e:Expr = m; e.isSimple()}
      case c:CalloutCall => {val e:Expr = c; e.isSimple()}
      case If(preStmts, condition, thenb, elseb) => {
        var preds: List[Boolean] = preStmts.map(_.isSimple()) ++
          thenb.stmts.map(_.isSimple())
        elseb.map(_.stmts.map(s => preds = preds :+ s.isSimple()))
        preds = preds :+ condition.isSimpleLoad()
        return all(preds)
      }
      case _:For => assert(false, "Unelaborated for loop encountered"); false // For loops should be converted to while loops
      case While(_, _, _, Some(limit)) => assert(false, "Unelaborated limited while loop encountered."); false // while loops with a limit should be converted to unlimited while loops
      case While(preStmts, condition, block, None) => {
        val preds: List[Boolean] = preStmts.map(_.isSimple()) ++
          block.stmts.map(_.isSimple()) :+
          condition.isSimpleLoad()
        return all(preds)
      }
      case Return(None) => true
      case Return(Some(e)) => e.isSimpleLoad()
      case Break() => true
      case Continue() => true
    }

    /* Converts a statement (possibly simple or non-simple) into a list of
     * equivalent statements that are all simple by generating
     * temporary variables for complex sub-expressions.
     */
    def flatten(tempGen: TempVarGen) : List[Statement] = stmt match {
      case Assignment(left@Store(_, None), right) => {
        val (stmts, finalExpr) = right.flatten(tempGen)
        return stmts :+ Assignment(left, finalExpr)
      }
      case Assignment(Store(from, Some(index)),  right) => {
        // Anything of the form a[b] = c gets converted to a form similar to:
        // t0 = c
        // a[t1] = t0
        val (indexStmts, finalIndexExpr) = index.flatten(tempGen)
        val (rightStmts, finalRightExpr) = right.flatten(tempGen)
        val tempIndex = tempGen.newIntVar()
        val tempRight = tempGen.newVarLike(right)
        return indexStmts ++ rightStmts :+
          Assignment(Store(tempIndex, None), finalIndexExpr) :+
          Assignment(Store(tempRight, None), finalRightExpr) :+
          Assignment(Store(from, Some(LoadField(tempIndex, None))), LoadField(tempRight, None))
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
        val condTemp = tempGen.newVar(DTBool)
        val newPreStmts = preStmts.flatMap(_.flatten(tempGen)) ++
          condStmts :+
          Assignment(Store(condTemp, None), condExpr)
        return List(If(newPreStmts,
          LoadField(condTemp, None),
          thenb.flatten(),
          elseb.map(_.flatten())
        ))
      }
      case _:For => assert(false, "Unelaborated for loop encountered."); List()
      // While loops with no limit are straight forward to convert
      case While(preStmts, cond, block, None) => {
        val (condStmts, condExpr) = cond.flatten(tempGen)
        val condTemp = tempGen.newVar(DTBool)
        val newPreStmts = preStmts.flatMap(_.flatten(tempGen)) ++
          condStmts :+
          Assignment(Store(condTemp, None), condExpr)
        return List(While(
          newPreStmts,
          LoadField(condTemp, None),
          block.flatten(),
          None
        ))
      }
      // Convert a while loop with a limit to one without a limit, by adding a temporary counter variable
      case While(_, _, _, Some(limit)) => assert(false, "Unelaborated limited while loop encountered."); List()
      case Return(None) => List(Return(None))
      case Return(Some(expr)) => {
        val (exprStmts, finalExpr) = expr.flatten(tempGen)
        val tempVar = tempGen.newVarLike(finalExpr)
        return (exprStmts :+
          Assignment(Store(tempVar, None), finalExpr)) :+
          Return(Some(LoadField(tempVar, None)))
      }
      case Break() => List(Break())
      case Continue() => List(Continue())
    }
  }

  implicit class MaybeSimpleBlock (var block: Block) {
    def flatten() : Block = {
      val tempGen = new TempVarGen(block.fields)
      val newStmts = block.stmts.flatMap(_.flatten(tempGen))
      return Block(newStmts, block.fields)
    }

    def isSimple() : Boolean = all(block.stmts.map(_.isSimple()))
  }
}
