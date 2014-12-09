package compile


object IR2 {
  import IRShared._
  import SymbolTable._

  // Earlier version constrain all fields to come before methods.
  // The callouts are discarded.
  case class Program(fields: List[FieldSymbol], main: Method, methods: List[Method]) {
    override def toString() = "%s\n%s\n%s".format(
      fields.mkString("\n"),
      main.toString,
      methods.mkString("\n\n")
    )
  }

  case class Field(id: ID, size: Option[Long]) {
    override def toString() = "%s%s".format(id,
      size match {
        case Some(l:Long) => "[" + l + "]"
        case None => ""
      }
    )
  }

  // symbols is the parameter symbol table.
  case class Method(id: ID, params: SymbolTable, locals: SymbolTable, cfg: CFG, returnType:DType ) {
    override def toString() = "%s %s()".format(returnType, id)
  }

  // Not the same as an IR block! Cannot contain any control flow statements.
  // The lecture notes from 10/9 explain the idea.
  class Block(val stmts: List[Statement], val loopHead:Boolean = false) {
    override def toString() = {
      val stmtString = stmts.map(_.toString.split('\n').mkString("\n  ")).mkString("\n  ")
      val uuid = SlugGenerator.id(this)
      "Block %s\n  %s".format(uuid, stmtString)
    }
  }

  object Block {
    def apply(stmts: List[Statement], loopHead:Boolean = false) = new Block(stmts, loopHead)
  }

  // Implicitly convert a list of statements to a block
  implicit def stmtsToBlock(stmts: List[Statement]) : Block = Block(stmts)

  // in IR2, Statements can not contain control flow.
  sealed trait Statement

  case class Assignment(left: FieldSymbol, right: Expr) extends Statement {
    override def toString = "%s = %s".format(left.id, right)
  }

  case class ArrayAssignment(left: FieldSymbol, index: Load, right: Load) extends Statement {
    override def toString = "%s[%s] = %s".format(left.id, index, right)
  }

  // Call create_and_run_threads on the function at label.
  case class SpawnThreads(label: String) extends Statement

  case class Call(id: ID, args:List[Either[StrLiteral, Load]]) extends Statement with Expr {
    override def toString = {
      def esc2(str: String): String = Escape.escape(Escape.escape(str))
      val q = Escape.escape('"')
      val argStrings = args.map{ _ match {
        case Left(StrLiteral(str)) => q + esc2(str) + q
        case Right(expr) => expr
      }}
      "%s(%s)".format(id, argStrings.mkString(", "))
    }
  }

  case class Return(value: Option[Load]) extends Statement {
    override def toString = value match {
      case Some(value) => s"return $value"
      case None => "return"
    }
  }

  sealed trait Expr
  case class BinOp(left: Load, op: BinOpType, right: Load) extends Expr {
    override def toString = "%s %s %s".format(left, op, right)
  }
  case class UnaryOp(op: UnaryOpType, right: Load) extends Expr {
    override def toString = "%s %s".format(op, right)
  }

  case class ArrayAccess(left: FieldSymbol, index: Load) extends Expr {
    override def toString = "%s[%s]".format(left.id, index)
  }

  sealed trait Load extends Expr
  case class LoadField(from: FieldSymbol) extends Load {
    override def toString = from.id
  }

  case class LoadLiteral(value: Long) extends Load {
    override def toString = value.toString
  }

  sealed trait Transition
  case class Edge(to: Block) extends Transition {
    override def toString() = "Edge to %s".format(SlugGenerator.id(to))
  }
  case class Fork(condition: Load, ifTrue: Block, ifFalse: Block) extends Transition {
    override def toString() = "Fork condition: %s\nTrue jump: %s\nFalse jump: %s".format(
      condition, ifTrue, ifFalse)
  }

  type EdgeMap = Map[IR2.Block, IR2.Transition]
}

