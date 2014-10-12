package compile;

object AsmGen {
  import scala.language.postfixOps

  implicit class DSLString(s: String) {
    // These are postfix because prefix was frustrating.
    def $(): String = "$%s".format(s);
    def %(): String = s"%$s";
    // Comments
    def ?(a: String): String = s"$s # $a\n"
    // Yeah.. the backslash is acts as both an op and
    // a line continuation, at least as far as I can tell.
    def \(a: String): String = s"$s\n$a"
    // - prefix for indentation
    def unary_-(): String = indent(s)

    override def toString = s.toString
    private def lines(strs: List[String]): String = if (!strs.nonEmpty) "" else strs.mkString("\n")
    private def indent(str: String): String = lines(str.split("\n").map("  " + _).toList)
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
    val stackbytes = stackvars * 8
    labl(label) \
    - push(rbp) \
    - mov(rsp, rbp) \
    - sub(stackbytes $, rsp) \
    - body \
    - add(stackbytes $, rsp) \
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
