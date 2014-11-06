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
    print("TRANSFORMING %s\n".format(method.id))
    val results = (new LiveVariables(method.cfg)).analyze()
    //val inputs = results.inputs
    val outputs = results.outputs

    def annotate(b: Block) : String = {
      val title = "%s outputs\n".format(b.uuid)
      val contents = outputs(b).toList.map(v => "%s\n".format(v)).mkString
      return title + contents
    }

    print(results)

    Grapher.graph(method, "deadcode", Some(annotate(_)))
    return method
  }
}
