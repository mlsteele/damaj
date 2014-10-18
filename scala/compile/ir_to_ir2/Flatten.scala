package compile

object Flatten {
  import IR._
  /*
   * Flattens DECAF statements so that all operators and function
   * calls operate only on loads, such that there are no nested
   * expressions.
   * This method mutates the input program, in addition to returning a new one.
   */
  def flatten(program: ProgramIR): ProgramIR = {
    return program
  }
}
