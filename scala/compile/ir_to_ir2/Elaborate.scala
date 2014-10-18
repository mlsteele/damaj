package compile

object Elaborate {
  import IR._
  import IRShared._
  import SymbolTable._
  import TempVarGen._
  import FunctionalUtils._

  /*
   * De-sugars some DECAF constructs into other, simpler DECAF constructs.
   * And/Or              -> if/else
   * Ternary conditional -> if/else
   * Limited while loops -> normal while loops
   * For loops           -> normal while loops
   * This method mutates the input program, in addition to returning a new one.
   */
  def elaborate(program: ProgramIR): ProgramIR = {
    val methods: List[MethodSymbol] = program.symbols.symbols.filter(_.isMethod()).asInstanceOf[List[MethodSymbol]]
    methods.map(m => m.block = m.block.elaborate())
    return program
  }

  implicit class ElaboratedExpr (expr: Expr) {
    def elaborate(tempGen: TempVarGen) : (List[Statement], Expr) = expr match {
      case _:Load => (List(), expr)
      case BinOp(left, And(), right) => {
        /*
         * a && b gets desugared to:
         * bool temp;
         * if (!a) {
         *   temp = false;
         * }
         * else {
         *   temp = b;
         * }
         */
        val tempVar = tempGen.newBoolVar()
        val thenStmt = Assignment(Store(tempVar, None), LoadBool(false))
        val elseStmt = Assignment(Store(tempVar, None), right)
        val ifStmt = If(
          List(),
          UnaryOp(Not(), left),
          newBlock(List(thenStmt)),
          Some(newBlock(List(elseStmt)))
        );
        return (ifStmt.elaborate(tempGen), LoadField(tempVar, None))
      }
      case BinOp(left, Or(), right) => {
        /*
         * a || b gets desugared to:
         * bool temp;
         * if (a) {
         *   temp = true;
         * }
         * else {
         *   temp = b;
         * }
         */
        val tempVar = tempGen.newBoolVar()
        val thenStmt = Assignment(Store(tempVar, None), LoadBool(true))
        val elseStmt = Assignment(Store(tempVar, None), right)
        val ifStmt = If(
          List(),
          left,
          newBlock(List(thenStmt)),
          Some(newBlock(List(elseStmt)))
        );
        return (ifStmt.elaborate(tempGen), LoadField(tempVar, None))
      }
      case BinOp(left, op, right) => {
        val (leftStmts, finalLeftExpr) = left.elaborate(tempGen)
        val (rightStmts, finalRightExpr) = right.elaborate(tempGen)
        return (
          leftStmts ++ rightStmts,
          BinOp(finalLeftExpr, op, finalRightExpr)
        )
      }
      case UnaryOp(op, operand) => {
        val (stmts, finalOperandExpr) = operand.elaborate(tempGen)
        return (
          stmts,
          UnaryOp(op, finalOperandExpr)
        )
      }
      case Ternary(cond, left, right) => {
        /*
         * c ? a : b gets desugared to:
         * if (c) {
         *   temp = a;
         * }
         * else {
         *   temp = b;
         * }
         */
        val tempVar = tempGen.newVarLike(left)
        val thenStmt = Assignment(Store(tempVar, None), left)
        val elseStmt = Assignment(Store(tempVar, None), right)
        val ifStmt = If(
          List(),
          cond,
          newBlock(List(thenStmt)),
          Some(newBlock(List(elseStmt)))
        );
        return (
          ifStmt.elaborate(tempGen),
          LoadField(tempVar, None)
        )
      }
      case MethodCall(symbol, args) => {
        var statements: List[Statement] = List()
        var tempVars: List[Expr] = List()
        for (eitherArg <- args) {
          // Extract the Rights out of the args, because why the hell can a method take a StringLiteral
          eitherArg.map (arg => {
            // elaborate all the args, collecting their dependency statements and temporary vars
            val (argStatements, argExpr) = arg.elaborate(tempGen)
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
              // elaborate all the args, collecting their dependency statements and temporary vars
              val (argStatements, argExpr) = arg.elaborate(tempGen)
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

  implicit class ElaboratedStatement(stmt: Statement) {
    def elaborate(tempGen: TempVarGen) : List[Statement] = stmt match {
      case Assignment(left, right) => {
        val (stmts, finalExpr) = right.elaborate(tempGen)
        return stmts :+ Assignment(left, finalExpr)
      }
      // MethodCall and CalloutCall have weird casts because they
      // are both Exprs and Statements
      case m:MethodCall => {
        val (stmts, finalExpr) = m.asInstanceOf[Expr].elaborate(tempGen)
        return stmts :+ finalExpr.asInstanceOf[MethodCall]
      }
      case c:CalloutCall => {
        val (stmts, finalExpr) = c.asInstanceOf[Expr].elaborate(tempGen)
        return stmts :+ finalExpr.asInstanceOf[CalloutCall]
      }
      case If(preStmts, cond, thenb, elseb) => {
        val (condStmts, condExpr) = cond.elaborate(tempGen)
        val newPreStmts = preStmts.flatMap(_.elaborate(tempGen)) ++
          condStmts
        return List(If(newPreStmts,
          condExpr,
          thenb.elaborate(),
          elseb.map(_.elaborate())
        ))
      }
      case For(id, start, iter, thenb) => tempGen.table.lookupSymbol(id) match {
        // Convert thr for loop into a while loop
        case None => assert(false, "Could not find ID in symbol table."); List()
        case Some(loopVar) => {
          // Generate statements needed to generate start value of loop variable
          val (startStmts, startExpr) = start.elaborate(tempGen)
          val preStmts = startStmts :+ Assignment(Store(loopVar.asInstanceOf[FieldSymbol], None), startExpr)
          // Generates statements needed to re-calculate ending expression each loop
          val (iterStmts, iterExpr) = iter.elaborate(tempGen)
          // Temp var for the right side of the condition
          val condVar = tempGen.newBoolVar()
          val condAssign = Assignment(Store(condVar, None), iterExpr)
          // While (loopVar != condVar)
          val condExpr = BinOp(LoadField(loopVar.asInstanceOf[FieldSymbol], None), NotEquals(), LoadField(condVar, None))
          // Generate statement to increment the loop var
          val varPlusOne = BinOp(LoadField(loopVar.asInstanceOf[FieldSymbol], None), Add(), LoadInt(1))
          val incVarStmt = Assignment(Store(loopVar.asInstanceOf[FieldSymbol], None), varPlusOne)
          // The original statements of the while loop, plus incVarStmt
          val loopStmts = thenb.elaborate().stmts :+ incVarStmt
          val whileLoop = While(iterStmts :+ condAssign,
            condExpr,
            Block(loopStmts, thenb.fields),
            None)
          return preStmts :+ whileLoop
        }
      }
      // While loops with no limit are straight forward to convert
      case While(preStmts, cond, block, None) => {
        val (condStmts, condExpr) = cond.elaborate(tempGen)
        val newPreStmts = preStmts.flatMap(_.elaborate(tempGen)) ++ condStmts
        return List(While(
          newPreStmts,
          condExpr,
          block.elaborate(),
          None
        ))
      }
      // Convert a while loop with a limit to one without a limit, by adding a temporary counter variable
      case While(preStmts, cond, block, Some(limit)) => {
        val counterVar = tempGen.newIntVar()
        val counterInit = Assignment(Store(counterVar, None), LoadInt(0))
        val counterInc = Assignment(
          Store(counterVar, None),
          BinOp(LoadField(counterVar, None),
            Add(),
            LoadInt(1))
        )
        val counterCondition = BinOp(
          LoadField(counterVar, None),
          Equals(),
          LoadInt(limit)
        )
        val newBlockStmts = block.stmts :+ counterInc
        return While(
          counterInit :: preStmts,
          BinOp(counterCondition, And(), cond),
          Block(newBlockStmts, block.fields),
          None).elaborate(tempGen)
      }
      case Return(None) => List(Return(None))
      case Return(Some(expr)) => {
        val (exprStmts, finalExpr) = expr.elaborate(tempGen)
        return exprStmts :+ Return(Some(finalExpr))
      }
      case Break() => List(Break())
      case Continue() => List(Continue())
    }
  }

  implicit class ElaboratedBlock (var block: Block) {
    def elaborate() : Block = {
      val tempGen = new TempVarGen(block.fields)
      val newStmts = block.stmts.flatMap(_.elaborate(tempGen))
      return Block(newStmts, block.fields)
    }
  }
}
