package compile;

// Example Usage:
//   val asm = new AsmGen(ir2).asm
class AsmGen(ir2: IR2.Program) {
  import AsmDSL._
  import scala.language.postfixOps
  import IRShared._
  import SymbolTable._
  import IR2._

  // Registers used for passing arguments to functions.
  val argregs = List(rdi, rsi, rdx, rcx, r8, r9)
  val argregc = argregs.length

  val reg_arridx = r10 // Register to store index into array.
  val reg_transfer = r11 // Register for intermediate value in assignment.
  val reg_opresult = r12 // Register for result of operations.
  val reg_divquo = rax // Division input and quotient
  val reg_divrem = rdx // Division input and remainder

  var strings = new StringStore()

  val asm = convertProgram(ir2)

  // TODO these should all be gone eventually.
  class AsmNotImplemented(message: String=null) extends RuntimeException(message)
  class AsmPreconditionViolated(message: String=null) extends RuntimeException(message)

  def convertProgram(ir2: IR2.Program): String = {
    val main = method("main",
      10, // TODO(miles): this needs to be a real number.
      convertCFG(ir2.main.cfg))
    val text = main
    val data = strings.toData
    file(text, data)
  }

  def convertCFG(cfg: CFG): String =
    convertCFGBlock(cfg.start, cfg)

  def convertCFGBlock(b: Block, cfg: CFG): String = {
    val next = cfg.edges(b) match {
      case None => ""
      case Some(Edge(next)) => convertCFGBlock(next, cfg)
      case Some(_:Fork) => throw new AsmNotImplemented()
    }
    convertBlock(b) \
    "" \
    next
  }

  def convertBlock(b: Block): String =
    b.stmts.map(stmt => convertStatement(stmt, b.fields)).mkString("\n")

  def convertStatement(stmt: Statement, symbols: SymbolTable): String = stmt match {
    case ir: IR.CalloutCall => convertCallout(ir, symbols)
    case ir: IR.Assignment => convertAssignment(ir, symbols)
    case ir => throw new AsmNotImplemented(ir.toString)
  }

  // TODO(miles): There's some ignoring of what type the ir access indices are.
  //              I think they're runtime enforced but I'm not really sure.
  def convertAssignment(ir: IR.Assignment, symbols: SymbolTable): String = (ir.left, ir.right) match {
    case (store: IR.Store, IR.LoadInt(v)) =>
      val wherestore = whereVar(store, symbols)
      wherestore.setup \
      mov(v $, wherestore.asmloc) ? "%s <- %s".format(store.to.id, v)
    case (store: IR.Store, load: IR.LoadField) =>
      val wherestore = whereVar(store, symbols)
      val whereload = whereVar(load, symbols)
      comment("%s <- %s".format(store.to.id, load.from.id)) \
      whereload.setup \
      mov(whereload.asmloc, reg_transfer) \
      wherestore.setup \
      mov(reg_transfer, wherestore.asmloc)
    case (store: IR.Store, IR.UnaryOp(op, right: IR.LoadField)) =>
      val wherestore = whereVar(store, symbols)
      val whereload = whereVar(right, symbols)
      val opinstr = op match {
        case _:Negative =>
          neg(reg_transfer) ? "negate"
        case _:Not => throw new AsmNotImplemented("Op not op should not make it to asmgen")
        case _:Length => throw new AsmPreconditionViolated("Op length should not make it to asmgen")
      }
      comment("%s <- (%s %s)".format(store.to.id, op, right.from.id)) \
      whereload.setup \
      mov(whereload.asmloc, reg_transfer) \
      opinstr \
      wherestore.setup \
      mov(reg_transfer, wherestore.asmloc)
    case (store: IR.Store, IR.BinOp(left: IR.LoadField, op, right: IR.LoadField)) =>
      val wherestore = whereVar(store, symbols)
      val whereleft = whereVar(left, symbols)
      val whereright = whereVar(right, symbols)
      val opinstr: String = op match {
        case _:Add =>
          add(reg_transfer, reg_opresult)
        case _:Subtract =>
          sub(reg_transfer, reg_opresult)
        case _:Multiply =>
          imul(reg_transfer, reg_opresult)
        case _:Divide =>
          mov(reg_opresult, reg_divquo) \
          mov(reg_opresult, reg_divrem) \
          idiv(reg_transfer) \
          mov(reg_divquo, reg_opresult)
        case _:Mod => throw new AsmNotImplemented()
        case _:LessThan => throw new AsmNotImplemented()
        case _:GreaterThan => throw new AsmNotImplemented()
        case _:LessThanEqual => throw new AsmNotImplemented()
        case _:GreaterThanEqual => throw new AsmNotImplemented()
        case _:Equals => throw new AsmNotImplemented()
        case _:NotEquals => throw new AsmNotImplemented()
        case _:And =>
          and(reg_transfer, reg_opresult)
        case _:Or =>
          or(reg_transfer, reg_opresult)
      }
      comment("%s <- (%s %s %s)".format(store.to.id, left.from.id, op, right.from.id)) \
      whereleft.setup \
      mov(whereleft.asmloc, reg_opresult) \
      whereright.setup \
      mov(whereright.asmloc, reg_transfer) \
      opinstr \
      wherestore.setup \
      mov(reg_opresult, wherestore.asmloc)
    case _ => throw new AsmNotImplemented(ir.toString)
  }

  def convertCallout(ir: IR.CalloutCall, symbols: SymbolTable): String = {
    ir.args
      .zipWithIndex
      .map(Function.tupled(convertCalloutArg(_,_,symbols)))
      .mkString("\n") \
    call(ir.callout.id)
  }

  def convertCalloutArg(arg: Either[StrLiteral, IR.Expr], argi: Int, symbols: SymbolTable): String = {
    arg match {
      // TODO escaping is probably broken
      case Left(StrLiteral(value)) =>
        mov(strings.put(value) $, whereArg(argi)) ? s"stage callout arg #$argi"
      case Right(IR.LoadInt(v)) =>
        throw new AsmNotImplemented()
        // mov(v $, argregs(argi)) ? s"stage callout arg #$argi"
      case Right(load: IR.LoadField) =>
        val whereload = whereVar(load, symbols)
        val whereargi = whereArg(argi)
        whereload.setup \
        mov(whereload.asmloc, whereargi) ? s"stage callout arg #$argi"
      case Right(e: IR.Expr) => throw new AsmNotImplemented(e.toString)
    }
  }

  // Get the location to stage an argument.
  def whereArg(argi: Int): String = argi < argregc match {
    case true => argregs(argi)
    case false => throw new AsmNotImplemented("high args")
  }

  // Represents how to access a variable location in assembly.
  // Setup must be executed after which asmloc is a valid argument.
  case class WhereVar(setup: String, asmloc: String)

  // Get the location of a symbol
  // Could clobber reg_arridx!
  // for example: "-32(%rbp)", "$coolglobal+8"
  def whereVar(id: ID, arrIdx: Option[IR.LoadField], symbols: SymbolTable): WhereVar = {
    // Optionally put the array index into reg_arridx.
    val setup = arrIdx match {
      case Some(IR.LoadField(from, None)) =>
        val w = whereVar(from.id, None, symbols)
        mov(w.asmloc, reg_arridx)
      case None => "nop"
      case _ => throw new AsmPreconditionViolated("array must be indexed by scalar field")
    }

    val offset = symbols.varOffset(id)
    val isArray = arrIdx.isDefined
    val asmLocation: String = (isArray, offset) match {
      case (false, LocalOffset(offIdx)) =>
        val off = -8 * (offIdx + 1)
        (rbp offset off)
      case (false, ArgOffset(offIdx)) if offIdx < argregc =>
        argregs(offIdx)
      case (false, ArgOffset(offIdx)) =>
        // TODO(miles): not tested.
        val off = 8 * (offIdx - argregc)
        (rbp offset off)
      case (false, GlobalOffset(offIdx)) => throw new AsmNotImplemented()
      case (true, _) => throw new AsmNotImplemented("arrays, hahahahha")
    }

    WhereVar(setup, asmLocation)
  }

  def whereVar(load: IR.LoadField, symbols: SymbolTable): WhereVar = load.index match {
    case Some(index: IR.LoadField) =>
      whereVar(load.from.id, Some(index), symbols)
    case None =>
      whereVar(load.from.id, None, symbols)
    case _ => throw new AsmPreconditionViolated("Array index must be field")
  }

  def whereVar(store: IR.Store, symbols: SymbolTable): WhereVar = store.index match {
    case Some(index: IR.LoadField) =>
      whereVar(store.to.id, Some(index), symbols)
    case None =>
      whereVar(store.to.id, None, symbols)
    case _ => throw new AsmPreconditionViolated("Array index must be field")
  }

  def example3: String = {
    val main =
      call("foo") \
      "" \
      comment("exit") \
      mov(0 $, rbx) ? "syscall: exit, status=rbx"
      mov(1 $, rax) \
      int(0x80 $)

    val foo =
      comment("say hello") \
      mov("hellostr" $, rdi) \
      call("printf") \
      "" \
      comment("say goodbye") \
      mov("goodbye" $, rdi) \
      call("printf")

    val text =
      method("main", 4, main) \
      method("foo", 4, foo)
    val data =
      datastring("hellostr", "hello world\\n") \
      datastring("goodbye", "goodbye\\n")
    file(text, data)
  }

  def example2: String = {
    val main =
      comment("say hello") \
      mov("hellostr" $, rdi) \
      call("printf") \
      "" \
      comment("say goodbye") \
      mov("goodbye" $, rdi) \
      call("printf") \
      "" \
      comment("exit") \
      mov(0 $, rbx) ? "syscall: exit, status=rbx"
      mov(1 $, rax) \
      int(0x80 $)

    val text =
      method("main", 4, main)
    val data =
      datastring("hellostr", "hello world\\n") \
      datastring("goodbye", "goodbye\\n")
    file(text, data)
  }


  def example: String = """
.text
.globl main
main:
    pushq %rbp
    movq %rsp, %rbp

    # say hello
    movq $hellostr, %rdi
    call printf

    # say goodbye
    movq $goodbye, %rdi
    call printf

    pushq $0
    popq %rbx

    pushq $0
    addq $8, %rsp

    subq $8, %rsp
    popq %rbx

    # exit
    movq $0, %rbx
    movq $1, %rax # syscall: exit, status=rbx
    int $0x80

.data
hellostr: .string "hello world.\n"
goodbye: .string "goodbye.\n"
""".drop(1)
}

// Mutable map for storing strings in the data segment of an assembly file.
class StringStore {
  import scala.collection.immutable.ListMap

  // map from strings to their labels
  private var store: ListMap[String, String] = ListMap()

  private def nextKey(): String =
    "str%s".format(store.size)

  // takes a string, returns it's label.
  // dose escaping.
  def put(s: String): String =
    putEscaped(Escape.escape(s))

  private def putEscaped(s: String): String = store.get(s) match {
    case Some(label) => label
    case None =>
      val key = nextKey()
      store += (s -> key)
      key
  }

  def toData: String = store.map{ case (payload, key) =>
    """%s: .string "%s"""".format(key, payload)
  }.mkString("\n")
}

object AsmDSL {
  import scala.language.postfixOps

  implicit class DSLString(s: String) {
    // These are postfix because prefix was frustrating.
    def $(): String = "$%s".format(s);
    def %(): String = s"%$s";
    def offset(offset: Int): String = s"$offset($s)";
    // Comments
    def ?(a: String): String = s"$s # $a"
    // Yeah.. the backslash is acts as both an op and
    // a line continuation, at least as far as I can tell.
    def \(a: String): String = s"$s\n$a"
    // - prefix for indentation
    def unary_-(): String = indent(s)

    override def toString = s.toString
    private def lines(strs: List[String]): String = if (!strs.nonEmpty) "" else strs.mkString("\n")
    private def indent(str: String): String = lines(str.split("\n").map("  " + _).toList)
  }

  implicit class DSLLong(s: Long) {
    def $(): String = "$%s".format(s);
  }

  implicit class DSLInt(s: Int) {
    def $(): String = "$%s".format(s);
  }

  // registers
  val (rax, rbx, rcx, rdx, rbp, rsp, rsi, rdi,
       r8, r9, r10, r11, r12, r13, r14, r15) =
      ("%rax", "%rbx", "%rcx", "%rdx", "%rbp", "%rsp", "%rsi", "%rdi",
       "%r8", "%r9", "%r10", "%r11", "%r12", "%r13", "%r14", "%r15")

  // instructions
  def mov(a: String, b: String): String = s"movq $a, $b"
  def call(a: String): String = s"call $a"
  def ret = "ret"
  def push(a: String): String = s"push $a"
  def pop(a: String): String = s"pop $a"
  def int(a: String): String = s"int $a"
  def add(a: String, b: String): String = s"addq $a, $b"
  def sub(a: String, b: String): String = s"subq $a, $b"
  def imul(a: String, b: String): String = s"imulq $a, $b"
  def idiv(a: String): String = s"idivq $a"
  def neg(a: String): String = s"negq $a"
  def and(a: String, b: String): String = s"andq $a, $b"
  def or(a: String, b: String): String = s"orq $a, $b"
  def xor(a: String, b: String): String = s"xorq $a, $b"

  // other assembly tools
  def labl(a: String): String = s"$a:"
  def datastring(label: String, contents: String): String =
    s"""$label: .string "$contents""""
  // whole line comment. See ? for inline comment
  def comment(a: String): String = s"# $a"

  // stackvars is the number of vars in the method's stack frame
  def method(label: String, stackvars: Int, body: String): String = {
    // TODO(miles): push and pop all regs.
    val stackbytes = stackvars * 8
    labl(label) \
    - push(rbp) \
    - mov(rsp, rbp) ? "set bp" \
    - sub(stackbytes $, rsp) ? s"reserve space for $stackvars locals" \
    - body \
    "" \
    - add(stackbytes $, rsp) ? s"free space from locals" \
    - pop(rbp) \
    - ret
  }

  def file(text: String, data: String): String = {
    ".text" \
    ".globl main" \
    text \
    "" \
    ".data" \
    data \
    ""
  }
}
