package compile

object DeadCodeElim {
  import IR2._

  def apply(program: IR2.Program): IR2.Program = {
    val newMethods = program.methods.map(transformMethod _)
    val newMain = transformMethod(program.main)
    return Program(
      program.fields,
      newMain,
      newMethods
    )
  }

  def transformMethod(method: Method) : Method = {
    print("TRANSFORMING %s".format(method.id))
    val results = (new ReachingDefinitions(method.cfg)).analyze()
    print(results)
    return method
  }
}
