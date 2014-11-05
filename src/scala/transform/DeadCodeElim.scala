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
    val results = (new ReachingDefinitions(method.cfg)).analyze()
    //val inputs = results.inputs
    val outputs = results.outputs

    def annotate(b: Block) : String = {
      val title = "%s outputs\n".format(b.uuid)
      var contents = ""
      for (ass <- outputs(b)) {
        contents += ass.toString + "\n"
      }
      return title + contents
    }

    Grapher.graph(method, "deadcode", Some(annotate(_)))
    return method
  }
}
