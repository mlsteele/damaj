package compile

object RegisterAllocation {
  import IR2._
  import SymbolTable._

  type Mappings = Map[Method, Map[FieldSymbol, String]]
  case class Result(program: Program, mappings: Mappings)

  private val freeregs = AsmGen.free_regs

  def apply(program:Program): Result =
    Result(program, allocateRegisters(program))

  private def allocateRegisters(program:Program): Mappings =
    Map(program.main -> allocateForMethod(program.main)) ++ program.methods.map{ 
      method => method -> allocateForMethod(method)
    }.toMap
  
  private def allocateForMethod(method:Method): Map[FieldSymbol, String] = {
    //val results = (new LiveVariables(method)).analyze()
    //val liveAfter = results.inputs
    //val liveBefore = results.outputs
    
    // lazy everything is connected
    val nodes = (method.params.getFields ++ method.locals.getFields).toSet
    def connected(node:FieldSymbol) = nodes

    GraphColor.color(nodes, connected, freeregs)
  }

}
