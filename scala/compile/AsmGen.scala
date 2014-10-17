package compile;

// Example Usage:
//   val asm = new AsmGen(ir2).asm
class AsmGen(ir2: IR2.Program) {
  import AsmDSL._
  import scala.language.postfixOps
  import IRShared._
  import SymbolTable._
  import IR2._

  val argregs = List(rdi, rsi, rdx, rcx, r8, r9)
  val argregc = argregs.length

  var strings = new StringStore()

  val asm = convertProgram(ir2)

  // TODO these should all be gone eventually.
  class AsmNotImplemented(message: String=null) extends RuntimeException(message)

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
    case (IR.Store(to, _), IR.LoadInt(v)) =>
      symbols.varOffset(to.id) match {
        case LocalOffset(idx: Int) =>
          val off = -8 * (idx + 1)
          mov(v $, rbp offset off) ? s"local[$idx] <- $v"
        case ArgOffset(idx: Int) => throw new AsmNotImplemented()
        case GlobalOffset(idx: Int) => throw new AsmNotImplemented()
      }
    case (IR.Store(to, _), IR.LoadField(from, _)) =>
      val offStore = symbols.varOffset(to.id)
      val offLoad = symbols.varOffset(from.id)
      (offStore, offLoad) match {
        case (LocalOffset(iS), LocalOffset(iL)) =>
          val offStore = -8 * (iS + 1)
          val offLoad = -8 * (iL + 1)
          "" ? s"local[$iS] <- local[$iL]" \
          push(r8) \
          mov(rbp offset offLoad, r8) \
          mov(r8, rbp offset offStore) \
          pop(r8)
        case _ => throw new AsmNotImplemented()
      }
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
      case Left(StrLiteral(value)) if argi < argregc =>
        mov(strings.put(value) $, argregs(argi)) ? s"prepare callout arg #$argi"
      case Left(StrLiteral(value)) =>
        throw new AsmNotImplemented(s"callouts with more than $argregc args")
      case Right(IR.LoadInt(v)) =>
        // TODO(miles): untested
        mov(v $, argregs(argi)) ? s"prepare callout arg #$argi"
      case Right(IR.LoadField(from, _)) =>
        symbols.varOffset(from.id) match {
          case LocalOffset(idx: Int) =>
            val off = -8 * (idx + 1)
            mov(rbp offset off, argregs(argi)) ? s"callout arg #$argi<- local[$idx]"
          case ArgOffset(idx: Int) => throw new AsmNotImplemented()
          case GlobalOffset(idx: Int) => throw new AsmNotImplemented()
        }
      case Right(e: IR.Expr) => throw new AsmNotImplemented(e.toString)
    }
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
  def sub(a: String, b: String): String = s"subq $a, $b"
  def add(a: String, b: String): String = s"addq $a, $b"

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
