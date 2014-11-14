package compile

object Inline {
  import IR2._
  def apply(program: Program) : Program = {
    return (new Inline(program)).transformProgram()
  }
}

private class Inline(program: IR2.Program) {
  import IR2._
  def transformProgram() = program
}
