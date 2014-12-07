package compile;

// Companion object for AsmGen class
object AsmGen {
  import AsmDSL._

  // Registers used for passing arguments to functions.
  val argregs = List(rdi, rsi, rdx, rcx, r8, r9)
  val argregc = argregs.length

  val reg_arridx = r10 // Register to store index into array.
  val reg_transfer = r11 // Register for intermediate value in assignment.
  val reg_opresult = rax // Register for result of operations.
  val reg_flagtarget = "%al" // Target byte reg for set ops. Bottom of %rax.
  val reg_divquo = rax // Division input and quotient
  val reg_divrem = rdx // Division input and remainder

  val special_regs: Seq[String] = List(reg_arridx, reg_transfer, reg_opresult, rax, reg_divquo, reg_divrem)
    .filter{ !argregs.contains(_) }
  val free_regs: Seq[String] = cpu_regs.filter{!special_regs.contains(_)}
}

// Example Usage:
//   val asm = new AsmGen(ir2).asm
class AsmGen(ir2: IR2.Program) {
  import AsmDSL._
  import AsmGen._
  import scala.language.postfixOps
  import scala.math.max
  import IRShared._
  import SymbolTable._
  import IR2._

  type ST = SymbolTable

  // Generates local labels
  val labelGenerator = new LabelGenerator()
  // TODO(miles): reserve more stuff
  labelGenerator.reserve("PUSH_ALL")
  labelGenerator.reserve("POP_ALL")
  labelGenerator.reserve(".data")
  labelGenerator.reserve("._exit1")
  // Store all string literals used in the program.
  val strings = new StringStore()
  // Keeps track of what Blocks have been assembled and their label.
  var assembledLabels = Map[Block, String]()

  val asm = generateProgram(ir2)

  class AsmPreconditionViolated(message: String=null) extends RuntimeException(message)

  def generateProgram(ir2: IR2.Program): String = {
    val main = generateMethod(ir2.main)

    val methods = ir2.methods.map(generateMethod).mkString("\n\n")
    val array_oob_error = "*** RUNTIME ERROR ***: Array out of Bounds access"
    val exits =
      labl("._exit1") \
      - "PUSH_ALL" \
      - mov(strings.put("array_oob_err", array_oob_error) $, argregs(0)) \
      - call("printf") \
      - "POP_ALL" \
      - mov(-1 $, argregs(0)) \
      - call("exit")

    val bss = ".data"\
      ir2.fields.flatMap { f:Field =>
        labl("decaf_global_%s".format(f.id))\
        - ".zero %d".format(f.size.getOrElse(1L)*8)\
        ""
      }.mkString

    val text = main \
              methods \
              exits

    val data = strings.toData
    file(bss, text, data)
  }

  // insert returns at the end of the method
  // if control reaches here something went wrong
  // if method should return a value, we go to a exit handler
  // if method shouldn't return a value, we return so we can go back to the caller
  def generateMethod(m: Method): String = {
    val numLocals = m.locals.size()
    // ABI requires even number of locals.. or something.
    val evenNumLocals = numLocals + (numLocals % 2)

    val body = generateCFG(m.cfg, m.locals, m.id + "_end")

    labelGenerator.reserve(m.id)
    return method(m.id, evenNumLocals, body)
  }

  // Generate assembly for a CFG.
  def generateCFG(cfg: CFG, symbols: ST, returnTo: String): String =
    generateCFG(cfg.start, cfg, symbols, returnTo)

  // Helper for generateCFG recursion.
  def generateCFG(b: Block, cfg: CFG, symbols: ST, returnTo: String): String = {
    val blockAsm = generateBlock(b, symbols, returnTo)
    val next = cfg.edges.get(b) match {
      case None => ""
      case Some(Edge(next)) =>
        assembledLabels.get(next) match {
          case Some(label) => - jmp(label)
          case None => generateCFG(next, cfg, symbols, returnTo)
        }
      case Some(Fork(condition, ifTrue, ifFalse)) =>
        val trueAsm = assembledLabels.get(ifTrue) match {
          case Some(label) => - jmp(label)
          case None => generateCFG(ifTrue, cfg, symbols, returnTo)
        }
        val falseLabel = labelGenerator.nextLabel("false")
        val falseAsm = assembledLabels.get(ifFalse) match {
          case Some(label) => - jmp(label)
          case None => generateCFG(ifFalse, cfg, symbols, returnTo)
        }
        val joinLabel = labelGenerator.nextLabel("join")
        val setup = condition match {
          case load: LoadField =>
            val whereload = whereVar(load, symbols)
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

  def generateBlock(b: Block, symbols: ST, returnTo: String): String = {
    assert(assembledLabels.get(b).isDefined == false)
    val blockAsm = b.stmts.map(stmt => generateStatement(stmt, symbols, returnTo)).mkString("\n")
    val label = labelGenerator.nextLabel("block")
    assembledLabels += (b -> label)
    labl(label) \
    - blockAsm
  }

  def generateStatement(stmt: Statement, symbols: ST, returnTo: String): String = stmt match {
    case ir: Call => generateCall(ir, symbols)
    case ir: Assignment => generateAssignment(ir, symbols)
    case ir: ArrayAssignment => generateArrayAssignment(ir, symbols)
    case ir: Return => generateReturn(ir, symbols, returnTo)
  }

  def generateArrayAssignment(assign: ArrayAssignment, symbols: ST) = assign.right match {
    case LoadLiteral(v) =>
      val wherefield = whereVar(assign.left.id, Some(assign.index), symbols)
      wherefield.setup \
      mov(v $, wherefield.asmloc)
    case load:LoadField =>
      val wherefield = whereVar(assign.left.id, Some(assign.index), symbols)
      val whereload = whereVar(load, symbols)
      whereload.setup \
      mov(whereload.asmloc, reg_transfer) \
      wherefield.setup \
      mov(reg_transfer, wherefield.asmloc)
  }

  def generateAssignment(ir: Assignment, symbols: ST): String = (ir.left, ir.right) match {
    case (field, LoadLiteral(v)) =>
      val wherefield = whereVar(field, symbols)
      wherefield.setup \
      mov(v $, wherefield.asmloc)
    case (field, load: LoadField) =>
      val wherefield = whereVar(field, symbols)
      val whereload = whereVar(load, symbols)
      whereload.setup \
      mov(whereload.asmloc, reg_transfer) \
      wherefield.setup \
      mov(reg_transfer, wherefield.asmloc)
    case (field, ArrayAccess(arrayField, index)) =>
      val wherefield = whereVar(field, symbols)
      val whereload = whereVar(arrayField.id, Some(index), symbols)
      whereload.setup \
      mov(whereload.asmloc, reg_transfer) \
      wherefield.setup \
      mov(reg_transfer, wherefield.asmloc)
    case (field, UnaryOp(op, right: Load)) =>
      val wherefield = whereVar(field, symbols)
      val whereload = whereVar(right, symbols)
      val opinstr = op match {
        case Negative =>
          neg(reg_transfer) ? "negate"
        case Not =>
          xor(1 $, reg_transfer) ? "not"
        case Length => throw new AsmPreconditionViolated("Op length should not make it to asmgen")
      }
      whereload.setup \
      mov(whereload.asmloc, reg_transfer) \
      opinstr \
      wherefield.setup \
      mov(reg_transfer, wherefield.asmloc)
    case (field, BinOp(left: Load, op, right: Load)) =>
      val wherefield = whereVar(field, symbols)
      val whereleft = whereVar(left, symbols)
      val whereright = whereVar(right, symbols)
      // Do (reg_opresult op reg_transfer) and field answer in reg_opresult
      val opinstr: String = op match {
        case Add =>
          add(reg_transfer, reg_opresult)
        case Subtract =>
          sub(reg_transfer, reg_opresult)
        case Multiply =>
          imul(reg_transfer, reg_opresult)
        case Divide =>
          // Uses auxillary reg_divquo and reg_divrem
          mov(reg_opresult, reg_divquo) \
          cqto \ // sign extend rax into rdx:rax
          idiv(reg_transfer) \
          mov(reg_divquo, reg_opresult)
        case Mod =>
          // Uses auxillary reg_divquo and reg_divrem
          mov(reg_opresult, reg_divquo) \
          cqto \ // sign extend rax into rdx:rax
          idiv(reg_transfer) \
          mov(reg_divrem, reg_opresult)
        case LessThan =>
          // All comparisons use reg_flagtarget
          cmp(reg_transfer, reg_opresult) \
          mov(0 $, reg_opresult) \
          setl(reg_flagtarget)
        case GreaterThan =>
          cmp(reg_transfer, reg_opresult) \
          mov(0 $, reg_opresult) \
          setg(reg_flagtarget)
        case LessThanEqual =>
          cmp(reg_transfer, reg_opresult) \
          mov(0 $, reg_opresult) \
          setle(reg_flagtarget)
        case GreaterThanEqual =>
          cmp(reg_transfer, reg_opresult) \
          mov(0 $, reg_opresult) \
          setge(reg_flagtarget)
        case Equals =>
          cmp(reg_transfer, reg_opresult) \
          mov(0 $, reg_opresult) \
          sete(reg_flagtarget)
        case NotEquals =>
          cmp(reg_transfer, reg_opresult) \
          mov(0 $, reg_opresult) \
          setne(reg_flagtarget)
        case And =>
          and(reg_transfer, reg_opresult)
        case Or =>
          or(reg_transfer, reg_opresult)
      }
      // Load left into reg_opresult
      // Load right into reg_transfer
      // Operations mostly use reg_opresult and reg_transfer internally.
      // Some operations like idiv use additional registers.
      whereleft.setup \
      mov(whereleft.asmloc, reg_opresult) \
      whereright.setup \
      mov(whereright.asmloc, reg_transfer) \
      opinstr \
      wherefield.setup \
      mov(reg_opresult, wherefield.asmloc)
    case (field, call:Call) => 
      val wherefield = whereVar(field, symbols)
      // wherefield must not modify rax.
      generateCall(call, symbols) \
      wherefield.setup \
      mov(rax, wherefield.asmloc)
  }

  def generateCall(ir: Call, symbols: ST): String = {
    val argMovs: String = ir.args
      .zipWithIndex
      .map(Function.tupled(generateCallArg(_,_,symbols)))
      .mkString("\n")
    // How much space to reserve on the stack for arguments.
    val stackArgsUnrounded = max(ir.args.length - argregc, 0)
    // Weird stack alignment thing.
    val stackArgs = stackArgsUnrounded + (stackArgsUnrounded % 2)

    comment("calling %s".format(ir.id)) \
    "PUSH_ALL" \
    sub((stackArgs * 8) $, rsp) ? s"reserve space for $stackArgs stack args" \
    argMovs \
    call(ir.id) \
    add((stackArgs * 8) $, rsp) ? s"free space from stack args" \
    "POP_ALL"
  }

  def generateReturn(ir: Return, symbols: ST, returnTo: String): String = ir.value match {
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
  }

  def generateCallArg(arg: Either[StrLiteral, Load], argi: Int, symbols: ST): String = {
    arg match {
      // TODO escaping is probably broken
      case Left(StrLiteral(value)) =>
        mov(strings.put(value) $, whereArg(argi)) ? s"stage callout arg #$argi"
      case Right(LoadLiteral(v)) =>
        mov(v $, whereArg(argi)) ? s"stage callout arg #$argi"
      case Right(load: LoadField) =>
        val whereload = whereVar(load, symbols)
        val whereargi = whereArg(argi)
        whereload.setup \
        mov(whereload.asmloc, reg_transfer) ? s"stage callout arg #$argi" \
        mov(reg_transfer, whereargi)
    }
  }

  // Get the location to stage an argument.
  // Assumes space has already been pushed onto stack for args.
  def whereArg(argi: Int): String = argi < argregc match {
    case true => argregs(argi)
    case false =>
      // Position above stack pointer
      val position = argi - argregc
      rsp offset (position * 8)
  }

  // Represents how to access a variable location in assembly.
  // .setup MUST be executed, after which asmloc is a valid argument.
  case class WhereVar(setup: String, asmloc: String)

  // Get the location of a symbol
  // Could clobber reg_arridx!
  // for example: "-32(%rbp)", "$coolglobal+8"
  def whereVar(id: ID, arrIdx: Option[Load], symbols: ST): WhereVar = {
    // Optionally put the array index into reg_arridx.
    val setup = arrIdx match {
      case Some(LoadField(from)) =>
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
      case None => ""
    }

    val offset = symbols.varOffset(id)
    val isArray = arrIdx.isDefined
    val asmLocation: String = (isArray, offset) match {
      case (false, LocalOffset(offIdx)) =>
        // Local Scalar
        val off = -8 * (offIdx + 1)
        (rbp offset off)
      case (false, ArgOffset(offIdx)) => offIdx < argregc match {
        case true =>
          // Scalar Argument in a arg register
          argregs(offIdx)
        case false =>
          // Scalar Argument on the stack
          val off = 8 * (offIdx - argregc + 2)
          (rbp offset off)
      }
      case (false, GlobalOffset(offIdx)) => {
        // Scalar Global
        "(decaf_global_%s)".format(id)
      }
      case (false, RegisterOffset(offIdx)) =>
        // Scalar Argument in a register
        free_regs(offIdx)
      case (true, LocalOffset(offIdx)) =>
        // Local Array
        val arraySize = symbols.lookupSymbol(id).get.asInstanceOf[FieldSymbol].size.get
        val offTop = (-8 * (offIdx))
        // Offset to the bottom of the array.
        val offBottom = offTop - 8 * arraySize
        // arrayAccess(displacement, baseReg, offsetReg, multiplierScalar)
        arrayAccess(offBottom.toString, rbp, reg_arridx, 8)
      case (true, GlobalOffset(offIdx)) => {
        // Global Array
        arrayAccess("decaf_global_%s".format(id), "", reg_arridx, 8)
      }
      case (true, _:ArgOffset) => throw new AsmPreconditionViolated("Arrays cannot be parameters")
      case (true, _:RegisterOffset) => throw new AsmPreconditionViolated("Arrays cannot be in registers")
    }

    WhereVar(setup, asmLocation)
  }

  def whereVar(load: LoadField, symbols: ST): WhereVar =
    whereVar(load.from.id, None, symbols)

  def whereVar(field: FieldSymbol, symbols: ST): WhereVar =
    whereVar(field.id, None, symbols)

  def whereVar(load: Load, symbols: ST): WhereVar = load match {
    case load:LoadField   => whereVar(load, symbols)
    case load:LoadLiteral => WhereVar("nop", load.value $)
  }

  def commentLoad(load: LoadField): String = load.from.id

  def commentLoad(load: Load): String = load match {
    case load@LoadField(_) => load.from.id
    case load@LoadLiteral(v) => s"$v"
  }
}

// Mutable map for storing strings in the data segment of an assembly file.
class StringStore {
  import scala.collection.immutable.ListMap

  // map from strings to their labels
  private var store: ListMap[String, String] = ListMap()

  private def nextKey(hint: String): String = hint match {
    case "" => "str%s".format(store.size)
    case _ => "str%s_%s".format(store.size, hint)
  }

  // takes a string, returns it's label.
  // does escaping.
  def put(s: String): String =
    putEscaped("", Escape.escape(s))

  def put(hint: String, s: String): String =
    putEscaped(hint, Escape.escape(s))

  private def putEscaped(hint: String, s: String): String = store.get(s) match {
    case Some(label) => label // discards hint, oh well.
    case None =>
      val key = nextKey(hint)
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

  def reserve(lbl: String): Unit = {
    assert(!taken.contains(lbl), s"Lable double-reserve $lbl")
    taken += lbl.toLowerCase
  }

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
  val cpu_regs: Seq[String] = List(rax, rbx, rcx, rdx, rbp, rsp, rsi, rdi,
                               r8, r9, r10, r11, r12, r13, r14, r15)

  // instructions
  def mov(a: String, b: String): String = s"movq $a, $b"
  def call(a: String): String = s"call $a"
  def ret = "ret"
  def push(a: String): String = s"pushq $a"
  def pop(a: String): String = s"popq $a"
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

  // sweet array access helper construct
  // see http://en.wikibooks.org/wiki/X86_Assembly/GAS_Syntax#Address_operand_syntax
  // Example Generated: (32 bit example)
  // -8(%ebp, %edx, 4) // access *(ebp - 8 + (edx * 4))
  def arrayAccess(displacement: String, baseReg: String, offsetReg: String, multiplier: Int) =
    s"$displacement($baseReg, $offsetReg, $multiplier)"

  // stackvars is the number of vars in the method's stack frame
  def method(label: String, stackvars: Int, body: String): String = {
    // TODO(miles): push and pop all regs.
    val stackbytes = stackvars * 8
    labl(label) \
    - push(rbp) \
    - mov(rsp, rbp) ? "set bp" \
    - sub(stackbytes $, rsp) ? s"reserve space for $stackvars locals" \
    - "PUSH_ALL" \
    body \
    labl(label + "_end") \
    - "POP_ALL" \
    - add(stackbytes $, rsp) ? s"free space from locals" \
    - pop(rbp) \
    - ret
  }

  def file(bss: String, text: String, data: String): String = {
    define_push_all \
    "" \
    define_pop_all \
    bss \
    "" \
    ".text" \
    ".globl main" \
    text \
    "" \
    ".data" \
    data \
    ""
  }

  // We push all registers in following order:
  // (rax, rbx, rcx, rdx, rbp, rsp, rsi, rdi,
  // r8, r9, r10, r11, r12, r13, r14, r15)
  // and pop in reverse order

  // This must push an even number of quads to keep rsp 16byte aligned.
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
    - push(r15) \ // duplicate for alignment
    ".endm"

  // This must pop an even number of quads to keep rsp 16byte aligned.
  def define_pop_all: String =
    ".macro POP_ALL" \
    - pop(r15) \ // duplicate for alignment
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
