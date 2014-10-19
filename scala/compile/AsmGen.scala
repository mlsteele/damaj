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
  val reg_opresult = rax // Register for result of operations.
  val reg_flagtarget = "%al" // Target byte reg for set ops. Bottom of %rax.
  val reg_divquo = rax // Division input and quotient
  val reg_divrem = rdx // Division input and remainder

  var strings = new StringStore()

  val asm = generateProgram(ir2)

  // TODO these should all be gone eventually.
  class AsmNotImplemented(message: String=null) extends RuntimeException(message)
  class AsmPreconditionViolated(message: String=null) extends RuntimeException(message)

  def generateProgram(ir2: IR2.Program): String = {
    val main = method("main",
      10, // TODO(miles): this needs to be a real number.
      generateCFG(ir2.main.cfg))
    val text = main
    val data = strings.toData
    file(text, data)
  }

  def generateCFG(cfg: CFG): String =
    generateCFGBlock(cfg.start, cfg)

  def generateCFGBlock(b: Block, cfg: CFG): String = {
    val next = cfg.edges(b) match {
      case None => ""
      case Some(Edge(next)) => generateCFGBlock(next, cfg)
      case Some(_:Fork) => throw new AsmNotImplemented()
    }
    generateBlock(b) \
    "" \
    next
  }

  def generateBlock(b: Block): String =
    b.stmts.map(stmt => generateStatement(stmt, b.fields)).mkString("\n")

  def generateStatement(stmt: Statement, symbols: SymbolTable): String = stmt match {
    case ir: Call => generateCall(ir, symbols)
    case ir: Assignment => generateAssignment(ir, symbols)
  }

  // TODO(miles): There's some ignoring of what type the ir access indices are.
  //              I think they're runtime enforced but I'm not really sure.
  def generateAssignment(ir: Assignment, symbols: SymbolTable): String = (ir.left, ir.right) match {
    case (store, LoadLiteral(v)) =>
      val wherestore = whereVar(store, symbols)
      wherestore.setup \
      mov(v $, wherestore.asmloc) ? "%s <- %s".format(store.to.id, v)
    case (store, load: LoadField) =>
      val wherestore = whereVar(store, symbols)
      val whereload = whereVar(load, symbols)
      comment("%s <- %s".format(store.to.id, load.from.id)) \
      whereload.setup \
      mov(whereload.asmloc, reg_transfer) \
      wherestore.setup \
      mov(reg_transfer, wherestore.asmloc)
    case (store, UnaryOp(op, right: LoadField)) =>
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
    case (store, BinOp(left: LoadField, op, right: LoadField)) =>
      val wherestore = whereVar(store, symbols)
      val whereleft = whereVar(left, symbols)
      val whereright = whereVar(right, symbols)
      // Do (reg_opresult op reg_transfer) and store answer in reg_opresult
      val opinstr: String = op match {
        case _:Add =>
          add(reg_transfer, reg_opresult)
        case _:Subtract =>
          sub(reg_transfer, reg_opresult)
        case _:Multiply =>
          imul(reg_transfer, reg_opresult)
        case _:Divide =>
          // Uses auxillary reg_divquo and reg_divrem
          mov(reg_opresult, reg_divquo) \
          cqto \ // sign extend rax into rdx:rax
          idiv(reg_transfer) \
          mov(reg_divquo, reg_opresult)
        case _:Mod =>
          // Uses auxillary reg_divquo and reg_divrem
          mov(reg_opresult, reg_divquo) \
          cqto \ // sign extend rax into rdx:rax
          idiv(reg_transfer) \
          mov(reg_divrem, reg_opresult)
        case _:LessThan =>
          // All comparisons use reg_flagtarget
          cmp(reg_transfer, reg_opresult) \
          mov(0 $, reg_opresult) \
          setl(reg_flagtarget)
        case _:GreaterThan =>
          cmp(reg_transfer, reg_opresult) \
          mov(0 $, reg_opresult) \
          setg(reg_flagtarget)
        case _:LessThanEqual =>
          cmp(reg_transfer, reg_opresult) \
          mov(0 $, reg_opresult) \
          setle(reg_flagtarget)
        case _:GreaterThanEqual =>
          cmp(reg_transfer, reg_opresult) \
          mov(0 $, reg_opresult) \
          setge(reg_flagtarget)
        case _:Equals =>
          cmp(reg_transfer, reg_opresult) \
          mov(0 $, reg_opresult) \
          sete(reg_flagtarget)
        case _:NotEquals =>
          cmp(reg_transfer, reg_opresult) \
          mov(0 $, reg_opresult) \
          setne(reg_flagtarget)
        case _:And =>
          and(reg_transfer, reg_opresult)
        case _:Or =>
          or(reg_transfer, reg_opresult)
      }
      // Load left into reg_opresult
      // Load right into reg_transfer
      // Operations mostly use reg_opresult and reg_transfer internally.
      // Some operations like idiv use additional registers.
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

  def generateCall(ir: Call, symbols: SymbolTable): String = {
    ir.args
      .zipWithIndex
      .map(Function.tupled(generateCallArg(_,_,symbols)))
      .mkString("\n") \
    call(ir.id)
  }

  def generateCallArg(arg: Either[StrLiteral, Expr], argi: Int, symbols: SymbolTable): String = {
    arg match {
      // TODO escaping is probably broken
      case Left(StrLiteral(value)) =>
        mov(strings.put(value) $, whereArg(argi)) ? s"stage callout arg #$argi"
      case Right(LoadLiteral(v)) =>
        throw new AsmNotImplemented()
        // mov(v $, argregs(argi)) ? s"stage callout arg #$argi"
      case Right(load: LoadField) =>
        val whereload = whereVar(load, symbols)
        val whereargi = whereArg(argi)
        whereload.setup \
        mov(whereload.asmloc, whereargi) ? s"stage callout arg #$argi"
      case Right(e: Expr) => throw new AsmNotImplemented(e.toString)
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

  def whereVar(load: LoadField, symbols: SymbolTable): WhereVar = load.index match {
    case Some(index: IR.LoadField) =>
      whereVar(load.from.id, Some(index), symbols)
    case None =>
      whereVar(load.from.id, None, symbols)
    // TODO remove _ case
    case _ => throw new AsmPreconditionViolated("Array index must be field")
  }

  def whereVar(store: Store, symbols: SymbolTable): WhereVar = store.index match {
    case Some(index: IR.LoadField) =>
      whereVar(store.to.id, Some(index), symbols)
    case None =>
      whereVar(store.to.id, None, symbols)
    case _ => throw new AsmPreconditionViolated("Array index must be field")
  }

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
  // Two's complement negation
  def neg(a: String): String = s"negq $a"
  def and(a: String, b: String): String = s"andq $a, $b"
  def or(a: String, b: String): String = s"orq $a, $b"
  def xor(a: String, b: String): String = s"xorq $a, $b"
  def cmp(a: String, b: String): String = s"cmpq $a, $b"
  def setg(a: String): String  = s"setg $a" // >
  def setge(a: String): String = s"setge $a" // >=
  def setl(a: String): String  = s"setl $a" // <
  def setle(a: String): String = s"setle $a" // <=
  def sete(a: String): String = s"sete $a" // ==
  def setne(a: String): String = s"setne $a" // !=
  def cqto(): String = "cqto" // convert quad to oct
  def exit1(): String = "jmp exit1" // jmp to exit1 code to jump with appropriate exit values
  def exit2(): String = "jmp exit2" // jmp to exit2 code that can handle control fall off
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
