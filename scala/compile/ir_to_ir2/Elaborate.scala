package compile

object Elaborate {
  import IR._
  import IRShared._
  import SymbolTable._
  import TempVarGen._

  /*
   * De-sugars some DECAF constructs into other, simpler DECAF constructs.
   * And/Or              -> nested conditionals
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
      case BinOp(left, op, right) => {
        // returns (statements, finalExpr)
        op match {
          case a:And => {
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
              UnaryOp(Not(), LoadField(tempVar, None)),
              newBlock(List(thenStmt)),
              Some(newBlock(List(elseStmt)))
            )
            return (List(ifStmt), LoadField(tempVar, None))
          }
          case _ =>
            (List(), BinOp(left, op, right))
        }
      }
      case _ =>
        return (List(), expr)
    }
  }

  implicit class ElaboratedStatement(stmt: Statement) {
    /* Converts a statement (possibly simple or non-simple) into a list of
     * equivalent statements that are all simple by generating
     * temporary variables for complex sub-expressions.
     */
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

  implicit class MaybeSimpleBlock (var block: Block) {
    def elaborate() : Block = {
      val tempGen = new TempVarGen(block.fields)
      val newStmts = block.stmts.flatMap(_.elaborate(tempGen))
      return Block(newStmts, block.fields)
    }
  }
}
