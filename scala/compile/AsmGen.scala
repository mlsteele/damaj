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

  // Generates local labels
  val labelGenerator = new LabelGenerator()
  // TODO(miles): reserve more stuff
  labelGenerator.reserve("main")
  labelGenerator.reserve("PUSH_ALL")
  labelGenerator.reserve("POP_ALL")
  // Store all string literals used in the program.
  val strings = new StringStore()
  // Keeps track of what Blocks have been assembled and their label.
  var assembledLabels = Map[Block, String]()

  val asm = generateProgram(ir2)

  class AsmPreconditionViolated(message: String=null) extends RuntimeException(message)
  // TODO these notimplemented exceptions should all be gone eventually.
  class AsmNotImplemented(message: String=null) extends RuntimeException(message)

  def generateProgram(ir2: IR2.Program): String = {
    val main = generateMethod(ir2.main)

    val methods = ir2.methods.map(generateMethod).mkString("/n")

    val array_oob_error = "*** RUNTIME ERROR ***: Array out of Bounds access"
    val control_runoff_error = "*** RUNTIME ERROR ***: Control fell off non-void method"
    val exits =
      labl("._exit1") \
      - mov(strings.put(array_oob_error) $, argregs(0)) \
      - "PUSH_ALL" \
      - call("printf") \
      - "POP_ALL" \
      - mov(-1 $, argregs(0)) \
      - call("exit") \
      labl("._exit2") \
      - mov(strings.put(control_runoff_error) $, argregs(0)) \
      - "PUSH_ALL" \
      - call("printf") \
      - "POP_ALL" \
      - mov(-2 $, argregs(0)) \
      - call("exit")

    val text = main \
              methods \
              exits
    val data = strings.toData
    file(text, data)
  }

// insert returns at the end of the method
// if control reaches here something went wrong
// if method should return a value, we go to a exit handler
// if method shouldn't return a value, we return so we can go back to the caller

  def generateMethod(m: Method): String = {
    val numLocals = m.symbols.size()
    // ABI requires even number of locals.. or something.
    val evenNumLocals = numLocals + (numLocals % 2)

    // Defend against control falling off the edge of the method.
    val controlCheck = m.returnType match {
      case DTVoid => "" // cool, no return needed
      case _ => jmp("._exit2")
    }
    val body = generateCFG(m.cfg, m.id + "_end") \ controlCheck
    return method(m.id, evenNumLocals, body)
  }

  // Generate assembly for a CFG.
  def generateCFG(cfg: CFG, returnTo: String): String =
    generateCFG(cfg.start, cfg, returnTo)

  // Helper for generateCFG recursion.
  def generateCFG(b: Block, cfg: CFG, returnTo: String): String = {
    val blockAsm = generateBlock(b, returnTo)
    val next = cfg.edges(b) match {
      case None => ""
      case Some(Edge(next)) =>
        assembledLabels.get(next) match {
          case Some(label) => - jmp(label)
          case None => generateCFG(next, cfg, returnTo)
        }
      case Some(Fork(condition, ifTrue, ifFalse)) =>
        val trueAsm = assembledLabels.get(ifTrue) match {
          case Some(label) => - jmp(label)
          case None => generateCFG(ifTrue, cfg, returnTo)
        }
        val falseLabel = labelGenerator.nextLabel("false")
        val falseAsm = assembledLabels.get(ifFalse) match {
          case Some(label) => - jmp(label)
          case None => generateCFG(ifFalse, cfg, returnTo)
        }
        val joinLabel = labelGenerator.nextLabel("join")
        // TODO(miles): check condition
        // TODO(miles): this doesn't work at all. I'm on it.
        val setup = condition match {
          case load: LoadField =>
            val whereload = whereVar(load, b.fields)
            whereload.setup \
            - cmp(0 $, whereload.asmloc)
          case LoadLiteral(v) =>
            - mov(v $, reg_transfer)\
            - cmp(0 $, reg_transfer)
        }

        setup \
        je(falseLabel) \
        trueAsm \
        - jmp(joinLabel) \
        labl(falseLabel) \
        falseAsm \
        labl(joinLabel)
    }

    blockAsm \
    next
  }

  def generateBlock(b: Block, returnTo: String): String = {
    assert(assembledLabels.get(b).isDefined == false)
    val blockAsm = b.stmts.map(stmt => generateStatement(stmt, b.fields, returnTo)).mkString("\n")
    val label = labelGenerator.nextLabel("block")
    assembledLabels += (b -> label)
    labl(label) \
    - blockAsm
  }

  def generateStatement(stmt: Statement, symbols: SymbolTable, returnTo: String): String = stmt match {
    case ir: Call => generateCall(ir, symbols)
    case ir: Assignment => generateAssignment(ir, symbols)
    case ir: Return => generateReturn(ir, symbols, returnTo)
  }

  def generateAssignment(ir: Assignment, symbols: SymbolTable): String = (ir.left, ir.right) match {
    case (store, LoadLiteral(v)) =>
      val wherestore = whereVar(store, symbols)
      wherestore.setup \
      mov(v $, wherestore.asmloc) ? "%s <- %s".format(commentStore(store), v)
    case (store, load: LoadField) =>
      val wherestore = whereVar(store, symbols)
      val whereload = whereVar(load, symbols)
      comment("%s <- %s".format(commentStore(store), commentLoad(load))) \
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
        case _:Not =>
          xor(1 $, reg_transfer) ? "not"
        case _:Length => throw new AsmPreconditionViolated("Op length should not make it to asmgen")
      }
      comment("%s <- (%s %s)".format(commentStore(store), op, commentLoad(right))) \
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
      comment("%s <- (%s %s %s)".format(commentStore(store), commentLoad(left), op, commentLoad(right))) \
      whereleft.setup \
      mov(whereleft.asmloc, reg_opresult) \
      whereright.setup \
      mov(whereright.asmloc, reg_transfer) \
      opinstr \
      wherestore.setup \
      mov(reg_opresult, wherestore.asmloc)
    case (store, call:Call) => 
      val wherestore = whereVar(store, symbols)
      wherestore.setup \
      generateCall(call, symbols) \
      mov(rax, wherestore.asmloc)
    case _ => throw new AsmNotImplemented(ir.toString)
  }

  def generateCall(ir: Call, symbols: SymbolTable): String = {
    ir.args
      .zipWithIndex
      .map(Function.tupled(generateCallArg(_,_,symbols)))
      .mkString("\n") \
    "PUSH_ALL" \
    call(ir.id) \
    "POP_ALL"
  }

  def generateReturn(ir: Return, symbols: SymbolTable, returnTo: String): String = ir.value match {
    case Some(LoadLiteral(v)) =>
      mov(v $, rax) ? "set return value to %s".format(v) \
      jmp(returnTo)
    case Some(load: LoadField) =>
      val whereload = whereVar(load, symbols)
      comment("set return value to %s".format(commentLoad(load))) \
      whereload.setup \
      mov(whereload.asmloc, rax) \
      jmp(returnTo)
    case None => jmp(returnTo)
    case _ => throw new RuntimeException("That return wasn't flattened enough!")
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
  // .setup MUST be executed, after which asmloc is a valid argument.
  case class WhereVar(setup: String, asmloc: String)

  // Get the location of a symbol
  // Could clobber reg_arridx!
  // for example: "-32(%rbp)", "$coolglobal+8"
  def whereVar(id: ID, arrIdx: Option[Load], symbols: SymbolTable): WhereVar = {
    // Optionally put the array index into reg_arridx.
    val setup = arrIdx match {
      case Some(LoadField(from, None)) =>
        val w = whereVar(from.id, None, symbols)
        val arraySize = symbols.lookupSymbol(id).get.asInstanceOf[FieldSymbol].size.get
        // NOTE: We ignore w.setup here because from is guaranteed to be a scalar.
        //
        // We want to check two things:
        // 1) an array access is less than array size
        // 2) an array access is greater than 0
        mov(w.asmloc, reg_arridx) \
        cmp(arraySize $, reg_arridx) \
        jge("._exit1") \
        cmp(0 $, reg_arridx) \
        jl("._exit1")
      case Some(LoadLiteral(value)) =>
        val arraySize = symbols.lookupSymbol(id).get.asInstanceOf[FieldSymbol].size.get
        mov(value $, reg_arridx) \
        cmp(arraySize $, reg_arridx) \
        jge("._exit1") \
        cmp(0 $, reg_arridx) \
        jl("._exit1")
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
      case (true, LocalOffset(offIdx)) =>
        val off = -8 * (offIdx + 1)
        // arrayAccess(displacement, baseReg, offsetReg, multiplierScalar)
        arrayAccess(off, rbp, reg_arridx, 8)
      case (true, _) => throw new AsmNotImplemented("arrays, hahahahha")
    }

    WhereVar(setup, asmLocation)
  }

  def whereVar(load: LoadField, symbols: SymbolTable): WhereVar =
    whereVar(load.from.id, load.index, symbols)

  def whereVar(store: Store, symbols: SymbolTable): WhereVar =
    whereVar(store.to.id, store.index, symbols)

  def commentLoad(load: LoadField): String = load.index match {
    case None => load.from.id
    case Some(LoadLiteral(index)) =>
      "%s[%s]".format(load.from.id, index)
    case Some(LoadField(index, _)) =>
      "%s[%s]".format(load.from.id, index.id)
  }

  def commentStore(store: Store): String = store.to.size match {
    case None => store.to.id
    case Some(size) => "%s[_]"
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
  // dos escaping.
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

// Generator for local labels (start with a dot)
// Case insensitive, so picky.
class LabelGenerator {
  // map from strings to their labels
  private var taken = Set[String]()
  private var counter = 0

  def reserve(lbl: String): Unit =
    taken += lbl.toLowerCase

  def nextLabel(): String = nextLabel("")

  def nextLabel(hint: String): String = {
    val lbl = "._node_%s_%s".format(counter, hint)
    counter += 1
    taken contains lbl.toLowerCase match {
      case false =>
        reserve(lbl)
        lbl
      case true => nextLabel()
    }
  }
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
  def jmp(a: String): String = s"jmp $a"
  def add(a: String, b: String): String = s"addq $a, $b"
  def sub(a: String, b: String): String = s"subq $a, $b"
  def imul(a: String, b: String): String = s"imulq $a, $b"
  def idiv(a: String): String = s"idivq $a"
  def je(a: String): String = s"je $a"
  def jge(a: String): String = s"jge $a"
  def jg(a: String): String = s"jg $a"
  def jle(a: String): String = s"jle $a"
  def jl(a: String): String = s"jl $a"
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
  // other assembly tools
  def labl(a: String): String = s"$a:"
  def datastring(label: String, contents: String): String =
    s"""$label: .string "$contents""""
  // whole line comment. See ? for inline comment
  def comment(a: String): String = s"# $a"
  def arrayAccess(displacement: Int, baseReg: String, offsetReg: String, multiplier: Int) =
    s"$displacement($baseReg, $offsetReg, $multiplier)"
  // We push all registers in following orde:
  // (rax, rbx, rcx, rdx, rbp, rsp, rsi, rdi,
  // r8, r9, r10, r11, r12, r13, r14, r15) 
  // and pop in reverse order
  
  // stackvars is the number of vars in the method's stack frame
  def method(label: String, stackvars: Int, body: String): String = {
    // TODO(miles): push and pop all regs.
    val stackbytes = stackvars * 8
    labl(label) \
    - push(rbp) \
    - mov(rsp, rbp) ? "set bp" \
    - sub(stackbytes $, rsp) ? s"reserve space for $stackvars locals" \
    "PUSH_ALL" \
    body \
    "" \
    labl(label + "_end") \
    "POP_ALL" \
    - add(stackbytes $, rsp) ? s"free space from locals" \
    - pop(rbp) \
    - ret
  }

  def file(text: String, data: String): String = {
    define_push_all \
    "" \
    define_pop_all \
    "" \
    ".text" \
    ".globl main" \
    text \
    "" \
    ".data" \
    data \
    ""
  }

  def define_push_all: String =
    ".macro PUSH_ALL" \
    - push(rbx) \
    - push(rcx) \
    - push(rdx) \
    - push(rbp) \
    - push(rsp) \
    - push(rsi) \
    - push(rdi) \
    - push(r8) \
    - push(r9) \
    - push(r10) \
    - push(r11) \
    - push(r12) \
    - push(r13) \
    - push(r14) \
    - push(r15) \
    ".endm"

  def define_pop_all: String =
    ".macro POP_ALL" \
    - pop(r15) \
    - pop(r14) \
    - pop(r13) \
    - pop(r12) \
    - pop(r11) \
    - pop(r10) \
    - pop(r9) \
    - pop(r8) \
    - pop(rdi) \
    - pop(rsi) \
    - pop(rsp) \
    - pop(rbp) \
    - pop(rdx) \
    - pop(rcx) \
    - pop(rbx) \
    ".endm"
}
