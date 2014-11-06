package compile

object DeadCodeElim {
  import IR2._

  def apply(program: Program): Program = {
    val newMethods = program.methods.map(transformMethod _)
    val newMain = transformMethod(program.main)
    return Program(
      program.fields,
      newMain,
      newMethods
    )
  }

  def transformMethod(method: Method) : Method = {
    val results = (new LiveVariables(method.cfg)).analyze()
    val inputs = results.inputs
    val outputs = results.outputs

    def annotate(b: Block) : String = {
      val inputsTitle = "=========\nINPUTS\n=========\n"
      val inputsString = inputs(b).mkString("\n") + "\n"
      val outputsTitle = "=========\nOUTPUTS\n=========\n"
      val outputsString = outputs(b).mkString("\n")
      return inputsTitle + inputsString + outputsTitle + outputsString
    }

    Grapher.graph(method, "deadcode.live", Some(annotate(_)))
    return method
  }
}
