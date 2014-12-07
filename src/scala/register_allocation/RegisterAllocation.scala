package compile

object RegisterAllocation {
  import IR2._
  import SymbolTable._

  val freeregs = AsmGen.free_regs
  val degree = freeregs.length

  def allocateRegisters(program:Program): Map[Method, Map[FieldSymbol, String]] = 
    Map(program.main -> allocateForMethod(program.main)) ++ program.methods.map{ 
      method => method -> allocateForMethod(method)
    }.toMap
  
  def allocateForMethod(method:Method): Map[FieldSymbol, String] = {
    //val results = (new LiveVariables(method)).analyze()
    //val liveAfter = results.inputs
    //val liveBefore = results.outputs
    
    // lazy everything is connected
    val nodes = (method.params.getFields ++ method.locals.getFields).toSet
    def connected(node:FieldSymbol) = nodes

    GraphColor.color(nodes, connected, freeregs)
  }

}
