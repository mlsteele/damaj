package compile

// namespace collisions import AST._
// namespace collisions import IR._
//
// Code primarily borromed from ASTTools.scala.
// variable names come from the naming convention
// established there
//
// Example Usage:
// IRBuilder(parseTree).ir match {
//   case Left(errors) => weep
//   case Right(ir) => rejoice
// }
//
//Construct an IR from a AST
class IRBuilder(input: AST.ProgramAST) {
  import collection.mutable.ListBuffer
  import SymbolTable._
  import IRShared._

  class IRConstructionException(msg: String) extends RuntimeException(msg)
  type SemanticError = String

  val srcmap = input.srcmap
  // Keep track of errors during construction
  // The presence of errors in this list will cause
  // ir to hold a failure.
  val errors = ListBuffer[SemanticError]()
  val ir: Either[List[SemanticError], IR.ProgramIR] = convertProgram(input)

  case class Context(symbols:SymbolTable, inLoop:Boolean, returnType:DType)

  def addError(msg: String): Unit =
    errors += "%s\n".format(msg)

  def addError(node: srcmap.Key, msg: String): Unit =
    errors += srcmap.report(node, msg)

  def convertProgram(ast: AST.ProgramAST): Either[List[SemanticError], IR.ProgramIR] = {
    val symbols = new SymbolTable()

    // callouts
    symbols
      .addSymbols(ast.callouts.map(x => srcmap.alias(x, CalloutSymbol(x.id))))
      .map{ conflict =>
        addError(conflict.second,
          "Duplicate callout declaration '%s'.".format(conflict.second.id))
      }

    // fields
    addFieldsToTable(symbols, ast.fields)

    // methods
    // convetMethodDecl already adds itself to the symbol table
    ast.methods.map(convertMethodDecl(_, Context(symbols, false, DTVoid)))

    val unchecked_ir = IR.ProgramIR(symbols)
    // post-process checks
    checkMainMethod(unchecked_ir)

    // check the errors list to see whether an ir would be valid.
    errors.toList match {
      case Nil => Right(IR.ProgramIR(symbols))
      case errors => Left(errors)
    }
  }

  // Adds fields to table.
  // Adds errors about poorly constructed fields.
  // Adds errors about conflicts.
  def addFieldsToTable(symbols: SymbolTable, fields: List[AST.FieldDecl]): Unit = {
    fields.map{ fd =>
      convertFieldDecl(fd) match {
        case Some(fs) =>
          srcmap.alias(fd, fs)
          symbols.addSymbol(fs)
        case None => None
      }
    }.flatten.map{
      conflict => addError(conflict.second,
        "Duplicate field declaration for '%s'".format(conflict.second.id))
    }
  }

  // Make sure the the main method exists.
  // And has the correct signature. (void main())
  // Adds stuff to errors if there are issues.
  def checkMainMethod(unchecked_ir: IR.ProgramIR): Unit = {
    // Verify main exists
    unchecked_ir.symbols.lookupSymbol("main") match {
      case Some(m: MethodSymbol) =>
        // Check arguments
        m.args match {
          case List() => // ok.
          case _ => addError(m, "Method 'main' must take no arguments.")
        }
      case _ => addError("No method 'main' found")
    }
  }

  def convertFieldDecl(ast: AST.FieldDecl): Option[FieldSymbol] = {
    ast.size match {
      case Some(lit) => convertIntLiteral(lit) match {
        case None => None
        case Some(v) if v <= 0 =>
          addError(ast, "Array field must have length > 0")
          None
        case Some(v) => Some(FieldSymbol(ast.dtype, ast.id, Some(v)))
      }
      case None => Some(FieldSymbol(ast.dtype, ast.id, None))
    }
  }

  def convertIntLiteral(ast: IntLiteral): Option[Long] = ast.value match {
    case v if v < Long.MinValue =>
      addError(ast, "Int literal is too damn negative.")
      None
    case v if v > Long.MaxValue =>
      addError(ast, "Int literal is too damn big.")
      None
    case v => Some(v.toLong)
  }

  // Returns Unit it adds itself to the ctx symbol table.
  def convertMethodDecl(meth: AST.MethodDecl, ctx: Context): Unit = {
    val paramsTable = new SymbolTable(ctx.symbols)
    val duplicate_args = paramsTable.addSymbols(meth.args.map(convertMethodDeclArg(_, ctx)))
    duplicate_args.map{ conflict =>
      addError(meth, "Duplicate argument '%s' for method '%s'".format(conflict.second.id, meth.id))
    }

    // partialy construct the method so we can insert it into the symbol table
    // this is neccessary in order for the block to be able to refer to the method for recursion.
    val partialMethod = MethodSymbol(meth.id, paramsTable, meth.returns, IR.Block(List(), paramsTable));
    srcmap.alias(meth, partialMethod)

    ctx.symbols.addSymbol(partialMethod) match {
      case None => {
        val childContext = Context(paramsTable, false, meth.returns)
        // Complete partial method.
        partialMethod.block = convertBlock(meth.block, true, childContext)
      }
      case Some(conflict) =>
        addError(meth, "There already exists a symbol with the name of this method '%s'"
          .format(conflict.second.id))
    }
  }

  def convertExpr(ast: AST.Expr, ctx:Context): Option[IR.Expr] = {
    val unchecked: Option[IR.Expr] = ast match {
      case AST.BinOp(left, op, right) =>
        (convertExpr(left, ctx), convertExpr(right, ctx)) match {
          case (Some(left), Some(right)) => Some(IR.BinOp(left, op, right))
          case _ => None
        }
      // @ is special because it must hold an load array on the right.
      case AST.UnaryOp("@", right) => getArrayLength(right, ctx)
      case AST.UnaryOp(op, right) =>
        convertExpr(right, ctx) match {
          case Some(right) => Some(IR.UnaryOp(op, right))
          case None => None
        }
      case AST.Ternary(condition, left, right) =>
        (convertExpr(condition, ctx), convertExpr(left, ctx), convertExpr(right, ctx)) match {
          case (Some(c), Some(l), Some(r)) => Some(IR.Ternary(c, l, r))
          case _ => None
        }
      case AST.Literal(l: IntLiteral) => convertIntLiteral(l).map(IR.LoadInt)
      case AST.Literal(CharLiteral(v)) => convertIntLiteral(IntLiteral(v)).map(IR.LoadInt)
      case AST.Literal(BoolLiteral(v)) => Some(IR.LoadBool(v))
      case AST.Location(id, index) => {
        val symbol: Option[Symbol] = ctx.symbols.lookupSymbol(id)
        symbol match {
          case None => addError(ast, "Unknown identifier '%s'".format(id)); None
          case Some(symbol) => symbol match {
            case field_symbol: FieldSymbol => {
              (field_symbol.isArray, index) match {
                case (true, None) =>
                  addError(ast, "Expression cannot be an array"); None
                case (false, Some(idx)) =>
                  addError(ast, "Cannot index into scalar"); None
                case (false, None) =>
                  Some(IR.LoadField(field_symbol, None))
                case (true, Some(idx)) =>
                  convertExpr(idx, ctx) match {
                    case None => None
                    case Some(idx) =>
                      Some(IR.LoadField(field_symbol, Some(idx)))
                  }
              }
            }
            case _ => addError(ast, "Location must refer to field"); None
          }
        }
      }
      case a:AST.MethodCall => convertMethodCall(a, ctx)
    }

    unchecked.map(ir => srcmap.alias(ast, ir)).flatMap(verifyExpr)
  }

  def verifyExpr(expr: IR.Expr): Option[IR.Expr] = expr match {
    case IR.BinOp(l,o,r) => o match {
      case _:EqualityBinOp =>
        if (typeOfExpr(l) != typeOfExpr(r)) {
          addError(expr, "Mismatched types for %s".format(o)); None
        } else {
          Some(expr)
        }
      case _:ArithmeticBinOp =>
        if (typeOfExpr(l) != DTInt || typeOfExpr(r) != DTInt) {
          addError(expr, "Operator %s requires int operands".format(o)); None
        } else {
          Some(expr)
        }
      case _:RelationalBinOp =>
        if (typeOfExpr(l) != DTInt || typeOfExpr(r) != DTInt) {
          addError(expr, "Operator %s requires int operands".format(o)); None
        } else {
          Some(expr)
        }
      case _:BooleanBinOp =>
        if (typeOfExpr(l) != DTBool || typeOfExpr(r) != DTBool) {
          addError(expr, "Operator %s requires boolean operands".format(o)); None
        } else {
          Some(expr)
        }
    }
    case IR.UnaryOp(o,r) => o match {
      case Not() =>
        if (typeOfExpr(r) != DTBool) {
          addError(expr, "Operator ! requires a boolean operand"); None
        } else {
          Some(expr)
        }
      case Length() => r match {
        case IR.LoadField(fs, Some(idx)) => Some(expr)
        case _ => addError(expr, "Operator @ requires an array operand"); None
      }
      case Negative() => typeOfExpr(r) match {
        case DTInt =>
          Some(expr)
        case _ =>
          addError(expr, "- operator must act on an integer expression"); None
      }
    }
    case IR.Ternary(c,l,r) => typeOfExpr(c) match {
      case DTBool =>
        if (typeOfExpr(l) == typeOfExpr(r)) {
          Some(expr)
        } else {
          addError(expr, "Second and third parts of ternary must match in type"); None
        }
      case _ =>
        addError(expr, "Ternary condition must be boolean"); None
    }
    case IR.LoadField(field, optIndex) => optIndex match {
      case Some(indexExpr) => field.size match {
        case Some(sizeOfField) => typeOfExpr(indexExpr) match {
          case DTInt => Some(expr)
          case _ =>
            addError(expr, "Array index must be type int"); None
        }
        case None =>
          addError(expr, "Brackets used on non-array type"); None
      }
      case None => Some(expr)
    }
    // Argument checking is being done by convertMethodCall
    case IR.MethodCall(ms, args) => ms.returns match {
      case DTVoid => addError(expr, "Void method used as value in expr"); None
      case _ => Some(expr)
    }
    case _ => Some(expr)
  }

  // Returns the length of an array if it exists in the context.
  // Adds errors if it returns None
  def getArrayLength(ast: AST.Expr, ctx: Context): Option[IR.LoadInt] = ast match {
    case AST.Location(id, idx) =>
      val symbol = ctx.symbols.lookupSymbol(id)
      (idx, symbol) match {
        case (_, None) =>
          addError(ast, "Unknown identifier '%s'".format(id)); None
        case (None, Some(FieldSymbol(dtype, id, Some(length)))) =>
          Some(IR.LoadInt(length))
        case (_) =>
          addError(ast, "Cannot get length of non-array"); None
      }
    case _ => addError(ast, "Cannot get length of non-location"); None
  }

  def convertMethodDeclArg(ast: AST.MethodDeclArg, ctx:Context): FieldSymbol =
    FieldSymbol(ast.dtype, ast.id, None)

  def convertStatement(ast:AST.Statement, ctx:Context): Option[IR.Statement] = ast match{
    case a: AST.Assignment => convertAssignment(a, ctx)
    case a: AST.MethodCall => convertMethodCall(a, ctx)
    case a: AST.If => convertIf(a, ctx)
    case a: AST.For => convertFor(a, ctx)
    case a: AST.While => convertWhile(a, ctx)
    case a: AST.Return => convertReturn(a, ctx)
    case AST.Break if ctx.inLoop => Some(IR.Break)
    case AST.Break => addError(ast, "break must be in a loop"); None
    case AST.Continue if ctx.inLoop => Some(IR.Continue)
    case AST.Continue => addError(ast, "continue must be in a loop"); None
  }

  def convertMethodCall(ast: AST.MethodCall, ctx:Context): Option[IR.Call] = {
    val oSymbol = ctx.symbols.lookupSymbol(ast.id)
    oSymbol match {
      case Some(s) => s match {
        case symbol:MethodSymbol =>
          val methodCallArgs = ast.args.map(convertMethodCallArg(_, ctx))
          detectBadArgs(methodCallArgs) match {
            case None => None // some args were invalid expressions.
            case Some(methodCallArgs) => {
              val arg_count_correct: Boolean = methodCallArgs.length == symbol.args.length
              val arg_types_correct_list: List[Boolean] = (symbol.args zip methodCallArgs).map{
                case (f:FieldSymbol, Right(expr)) => f.dtype == typeOfExpr(expr) match {
                  case true => true
                  case false => addError(ast, "Method called with mismatched argument type"); false
                }
                case (f:FieldSymbol, Left(expr)) =>
                  addError(ast, "Method cannot be called with a string literal argument"); false
              }
              val arg_types_correct: Boolean = !arg_types_correct_list.filter(!_).nonEmpty

              (arg_count_correct, arg_types_correct) match {
                case (true, true) => Some(IR.MethodCall(symbol, methodCallArgs))
                case (false, true) =>
                  addError(ast, "Method called with wrong number of arguments"); None
                case (_, false) => None
              }
            }
          }
        case c:CalloutSymbol =>
          val methodCallArgs = ast.args.map(convertMethodCallArg(_, ctx))
          detectBadArgs(methodCallArgs) match {
            case None => None // some args were invalid expressions.
            case Some(methodCallArgs) => Some(IR.CalloutCall(c, methodCallArgs))
          }
        case f:FieldSymbol =>
          addError(ast, "Cannot call a field"); None
      }
      case None =>
        addError(ast, "Method '%s' does not exist".format(ast.id)); None
    }
  }

  // returns whether some args errored.
  def detectBadArgs(args: List[Either[StrLiteral, Option[IR.Expr]]]): Option[List[Either[StrLiteral, IR.Expr]]] = {
    val cleanargs_leveled: List[Option[Either[StrLiteral, IR.Expr]]] = args.map{ arg => arg match {
      case Left(strlit) => Some(Left(strlit))
      case Right(Some(expr)) => Some(Right(expr))
      case Right(None) => None
    } }

    val cleanargs: List[Either[StrLiteral, IR.Expr]] = cleanargs_leveled.flatten

    (cleanargs.length == args.length) match {
      case true => Some(cleanargs)
      case false => None
    }
  }

  def convertMethodCallArg(ast: Either[StrLiteral, AST.Expr], ctx:Context): Either[StrLiteral, Option[IR.Expr]] = {
    ast match {
      case Left(x) => Left(x)
      case Right(x) => Right(convertExpr(x, ctx))
    }
  }

  def convertAssignment(assign: AST.Assignment, ctx:Context): Option[IR.Assignment] = {
    val store = locToStore(assign.left, ctx)
    val rhs = convertExpr(assign.right, ctx)
    (store, rhs) match {
      case (Some(store), Some(rhs)) =>
        (store.to.dtype == typeOfExpr(rhs)) match {
          case false =>
            addError(assign, "Left and right sides of assignment must be the same type"); None
          case true =>
            store.index match {
              // normal variable assignment
              case None => Some(IR.Assignment(store, rhs))
              // Array access
              case Some(i) =>
                (typeOfExpr(i) == DTInt, store.to.isArray) match {
                  case (false, _) => addError(assign, "Array index must be an integer"); None
                  case (_, false) => addError(assign, "Cannot index into scalar"); None
                  case (true, true) => Some(IR.Assignment(store, rhs))
                }
            }
        }
      case (_, _) => None
    }
  }

  def locToStore(loc: AST.Location, ctx: Context): Option[IR.Store] = {
    val field = ctx.symbols.lookupSymbol(loc.id)
    field match {
      case Some(field: FieldSymbol) if field.size.isDefined && !loc.index.isDefined =>
        addError(loc, "Cannot assign to array"); None
      case Some(field: FieldSymbol) =>
        loc.index match {
          // Scalar store
          case None => Some(IR.Store(field, None))
          // Array store
          case Some(index) =>
            convertExpr(index, ctx) match {
              case None => None
              case Some(index) => Some(IR.Store(field, Some(index)))
            }
        }
      case Some(_) => addError(loc, "Cannot assign to non-field"); None
      case None => addError(loc, "Unknown identifier '%s'".format(loc.id)); None
    }
  }

  def convertBlock(block: AST.Block, ctx: Context): IR.Block =
    convertBlock(block, false, ctx)

  def convertBlock(block: AST.Block, isMethodBlock: Boolean, ctx: Context): IR.Block = {
    // methodBlock is whether this block is the top-level block of a method.
    val paramstable = ctx.symbols
    val localtable = new SymbolTable(ctx.symbols)

    // Immediate locals cannot shadow parameters
    isMethodBlock match {
      case true =>
        block.decls.filter{ decl =>
          // Make sure no locals are shadowing parameters.
          paramstable.lookupSymbolLocal(decl.id) match {
            case None => true
            case Some(_) => addError(decl, "Method block local cannot shadow parameter"); false
          }
        }.map{ decl =>
          addFieldsToTable(localtable, List(decl))
        }
      case false =>
        addFieldsToTable(localtable, block.decls)
    }

    // flatten is used here to drop the None's from the stmt list.
    val stmt_context = Context(localtable, ctx.inLoop, ctx.returnType)
    val stmts = block.stmts.map(convertStatement(_, stmt_context)).flatten
    IR.Block(stmts, localtable)
  }

  def convertIf(iff: AST.If, ctx:Context): Option[IR.If] = {
    val condition: Option[IR.Expr] = convertExpr(iff.condition, ctx)
    val thenBlock: IR.Block = convertBlock(iff.thenb, ctx)
    val elseBlock: Option[IR.Block] = iff.elseb.map(convertBlock(_, ctx))

    condition match {
      case None => None
      case Some(condition) =>
        typeOfExpr(condition) match {
          case DTBool => Some(IR.If(condition, thenBlock, elseBlock))
          case _ =>
            addError(iff.condition, "If condition must be a boolean"); None
        }
    }
  }

  def convertFor(fo: AST.For, ctx:Context): Option[IR.Statement] = ctx.symbols.lookupSymbol(fo.id) match {
    case None => addError(fo, "Symbol not found '%s'".format(fo.id)); None
    case Some(field) => field match {
      case field: FieldSymbol => {
        val start: Option[IR.Expr] = convertExpr(fo.start, ctx)
        val end: Option[IR.Expr] = convertExpr(fo.iter, ctx)
        val block: IR.Block = convertBlock(fo.thenb, Context(ctx.symbols, true, ctx.returnType))

        (start, end, field.dtype == DTInt) match {
          case (None, _, _) => None // invalid start
          case (_, None, _) => None // invalid end
          case (_, _, false) => addError(fo, "Must loop over int var"); None
          case (Some(start), _, _) if typeOfExpr(start) != DTInt =>
            addError(fo, "For loop start must be an int expression"); None
          case (_, Some(end), _) if typeOfExpr(end) != DTInt =>
            addError(fo, "For loop end must be an int expression"); None
          case (Some(start), Some(end), true) =>
            Some(IR.For(fo.id, start, end, block))
        }
      }
      case _ => addError(fo, "Loop variable must be a field"); None
    }
  }

  def convertWhile(whil: AST.While, ctx:Context): Option[IR.While] = {
    val condition: Option[IR.Expr] = convertExpr(whil.condition, ctx)
    // Note: We can use flatMap here because convertIntLiteral will add errors
    // and we want to create the while even if the intliteral is invalid.
    val max: Option[Long] = whil.max.flatMap(convertIntLiteral)
    val block = convertBlock(whil.block, Context(ctx.symbols, true, ctx.returnType))

    condition match {
      case None => None
      case Some(condition) =>
        typeOfExpr(condition) match {
          case DTBool => Some(IR.While(condition, block, max))
          case _ =>
            addError(whil.condition, "While condition must be a boolean"); None
        }
    }
  }

  def convertReturn (ret:AST.Return, ctx:Context): Option[IR.Statement] = {


    ret.expr match {
      case None => ctx.returnType match {
        case DTVoid => Some(IR.Return(None))
        case _ =>
          addError(ret, "Non-void method requires a return value"); None
      }
      case Some(expr) => convertExpr(expr, ctx) match {
        case None => None
        case Some(expr) => typeOfExpr(expr) match {
          case ctx.returnType => Some(IR.Return(Some(expr)))
          case _ =>
            addError(ret, "Return type must match the method signature"); None
        }
      }
    }
  }

}
