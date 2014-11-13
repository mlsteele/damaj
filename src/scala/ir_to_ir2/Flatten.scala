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
    val methods: List[MethodSymbol] = program.symbols.getMethods
    methods.map {m => 
      m.block = m.block.flatten()
    }
    for (m <- methods) {
      assert(m.block.isSimple(),
        "Method %s was not correctly simplified!\n%s".format(m.id, printMethod(m)))
      }
    return program
  }

  // Calls need to be handled specially, because they are both statements and exprs
  private implicit class FlatCall (call: Call) {
    def flattenCall(tempGen: TempVarGen): (List[Statement], Call) = call match {
      case MethodCall(symbol, args) => {
        var statements: List[Statement] = List()
        var newArgs: List[Load] = List()
        for (eitherArg <- args) {
          // Extract the Rights out of the args, because why the hell can a method take a StringLiteral
          eitherArg.map {arg =>
            // flatten all the args, collecting their dependency statements and temporary vars
            val (argStatements, argVar) = arg.flatten(tempGen)
            statements ++= argStatements
            // AsmGen cannot handle current function parameters being
            // passed to a call, so we make a temporary var for all
            // variables which are a parameter to the current function
            val methodTable = tempGen.table.methodTable.get
            argVar match {
              case LoadField(from, _) if (methodTable.symbols contains from) => {
                val tempVar:FieldSymbol = tempGen.newVarLike(argVar)
                val tempAssign = Assignment(tempVar, argVar)
                statements :+= tempAssign
                newArgs :+= fieldToLoad(tempVar)
              }
              case _ => newArgs :+= argVar
            }
          }
        }
        return (
          statements,
          MethodCall(symbol, newArgs.map(Right(_)))
        )
      }

      case CalloutCall(symbol, args) => {
        var statements: List[Statement] = List()
        var newArgs: List[Either[StrLiteral, Expr]] = List()
        for (eitherArg <- args) {
          eitherArg match {
            case Right(arg) => {
              // flatten all the args, collecting their dependency statements and temporary vars
              val (argStatements, argVar) = arg.flatten(tempGen)
              statements ++= argStatements
              // AsmGen cannot handle current function parameters being
              // passed to a call, so we make a temporary var for all
              // variables which are a parameter to the current function
              val methodTable = tempGen.table.methodTable.get
              argVar match {
                case LoadField(from, _) if (methodTable.symbols contains from) => {
                  val tempVar:FieldSymbol = tempGen.newVarLike(argVar)
                  val tempAssign = Assignment(tempVar, argVar)
                  statements :+= tempAssign
                  newArgs :+= Right(fieldToLoad(tempVar))
                }
                case _ => newArgs :+= Right(argVar)
              }
            }
            case Left(str) => {
              newArgs :+= Left(str)
            }
          }
        }
        return (
          statements,
          CalloutCall(symbol, newArgs)
        )
      }
    }
  }

  private implicit class MaybeSimpleExpr (expr: Expr) {
    def isSimpleLoad() : Boolean = expr match {
      case l:Load => l match {
        case LoadField(_, None) => true
        case LoadField(_, Some(_:LoadInt)) => false
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
      case l:Load => l.isSimpleLoad()
    }

    /**
      * Takes an expression, and generates a list of statements and
      * temporary variable assignments that are needed to generate the
      * expression, and a load containing the temporary variable the final output is in.
      * (z = a + b + c + d) will be translated into the following statements
      * and final expr:
      * statements = [t1 = a + b, t2 = t1 + c, t3 = t2 + d]
      * final load: (t3)
      */
    def flatten(tempGen: TempVarGen) : (List[Statement], Load) = expr match {
      case load@LoadField(from, Some(index)) => {
        // Flatten the index
        val (indexStmts, indexLoad) = index.flatten(tempGen)
        val tempVar = tempGen.newVarLike(load)
        // temp = array[index_temp]
        val tempAssign = Assignment(tempVar, LoadField(from, Some(indexLoad)))
        return (
          indexStmts :+ tempAssign,
          tempVar
        )
      }

        // No need to flatten a scalar load
      case l:Load => (List(),l)

      case UnaryOp(op, e) => {
        // Flatten the operand, and assign it to a temporary var
        val (operandStatements, operandVar) = e.flatten(tempGen)
        //val tempVar = tempGen.newVar(op.operandType())
        val tempVar = tempGen.newVar(op.returnType)
        val tempAssign = Assignment(tempVar, UnaryOp(op, operandVar))
        return (
          operandStatements :+ tempAssign,
          tempVar
        )
      }

      case BinOp(left, op, right) => {
        val (leftStmts, leftVar) = left.flatten(tempGen)
        val (rightStmts, rightVar) = right.flatten(tempGen)

        val tempVar = tempGen.newVar(op.returnType)
        val tempAssign = Assignment(tempVar, BinOp(leftVar, op, rightVar))

        return (
          leftStmts ++ rightStmts :+ tempAssign,
          tempVar
        )
      }

      case _:Ternary => {
        assert(false, "Flatten should never get a Ternary op.");
        return (List(), LoadInt(0))
      }

      case c:Call => {
        // Use special flattenCall method, and interpret the return type as an expr
        val (stmts, newCall) = c.flattenCall(tempGen)
        val tempVar = tempGen.newVarLike(newCall)
        val tempAssign = Assignment(tempVar, newCall)
        return (
          stmts :+ tempAssign,
          tempVar
        )
      }
    }
  }

  private implicit class MaybeSimpleStatement(stmt: Statement) {
    /**
      * Determines whether a statement is simple.
      * This means that any arguments or operands to a statement
      * are only Loads.
      */
    def isSimple() : Boolean = stmt match {
      // scalar = array[load] is simple
      case Assignment(Store(_, None), LoadField(_, Some(_:Load))) => true
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
        val (indexStmts, indexVar) = index.flatten(tempGen)
        val (rightStmts, rightVar) = right.flatten(tempGen)
        return indexStmts ++ rightStmts :+ Assignment(Store(from, Some(indexVar)), rightVar)
      }
      // Use special flattenCall method, and interpret the result as a statement
      case c:Call => {
        val (stmts, newCall) = c.flattenCall(tempGen)
        return stmts :+ newCall
      }
      case If(preStmts, cond, thenb, elseb) => {
        val (condStmts, condVar) = cond.flatten(tempGen)
        val newPreStmts = preStmts.flatMap(_.flatten(tempGen)) ++
          condStmts
        return List(If(newPreStmts,
          condVar,
          thenb.flatten(),
          elseb.map(_.flatten())
        ))
      }
      case _:For => assert(false, "Unelaborated for loop encountered."); List()
      // While loops with no limit are straight forward to convert
      case While(preStmts, cond, block, None) => {
        val (condStmts, condVar) = cond.flatten(tempGen)
        val newPreStmts = preStmts.flatMap(_.flatten(tempGen)) ++
          condStmts
        return List(While(
          newPreStmts,
          condVar,
          block.flatten(),
          None
        ))
      }
      // Convert a while loop with a limit to one without a limit, by adding a temporary counter variable
      case While(_, _, _, Some(limit)) => assert(false, "Unelaborated limited while loop encountered."); List()
      case Return(None) => List(Return(None))
      case r@Return(Some(expr)) if expr.isSimpleLoad() => List(r)
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

  private implicit class MaybeSimpleBlock (var block: Block) {
    def flatten() : Block = {
      val tempGen = new TempVarGen(block.fields)
      val newStmts = block.stmts.flatMap(_.flatten(tempGen))
      return Block(newStmts, block.fields)
    }

    def isSimple() : Boolean = all(block.stmts.map { _.isSimple() })
  }

  private implicit def fieldToStore(field: FieldSymbol) : Store = Store(field, None)

  private implicit def fieldToLoad(field: FieldSymbol) : Load = LoadField(field, None)
}
