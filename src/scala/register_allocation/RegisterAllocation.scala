package compile

object RegisterAllocation {
  import IR2._
  import SymbolTable._
  import AsmGen.free_regs

  // Mutates program.
  def apply(program:Program): Unit = allocateRegisters(program)

  private def allocateRegisters(program:Program): Unit =
    (program.main +: program.methods).foreach(installAllocationsForMethod)

  // Mutates the methods symbol table
  private def installAllocationsForMethod(method:Method): Unit = {
    val coloring = allocateForMethod(method)
    // Console.err.println("method: %s.id".format(method.id))
    // Console.err.println(coloring.map{
      // case (r, c) => "%s -> %s (%s)".format(r.id, c.index, AsmGen.free_regs(c.index))
    // }.mkString("\n"))
    method.locals.installRegisterAssignments(coloring)
  }

  private def allocateForMethod(method:Method): Map[FieldSymbol, RegisterOffset] = {
    val results = (new LiveVariables(method)).analyze()
    val liveAfter = results.inputs
    val liveBefore = results.outputs

    // Allocate for all scalar non-arg fields.
    val nodes = method.locals.getScalarFields.toSet

    // Every pair that is live at the same time is connected in this graph
    var connections:Map[FieldSymbol, Set[FieldSymbol]] = nodes.map{ _ -> Set[FieldSymbol]() }.toMap
    List(liveBefore, liveAfter).foreach{ _.foreach{ case (block, loadSet) =>
      loadSet.foreach{ load =>
        if (connections.get(load.from).isDefined) {
          val currentNeighbors = connections(load.from)
          val newNeighbors = loadSet.map(_.from) - load.from
          connections += (load.from -> (currentNeighbors ++ newNeighbors))
        }
      }
    }}

    val colors = (new Range(0, free_regs.size, 1)).map{ RegisterOffset(_) }
    def neighbors(node:FieldSymbol) = connections(node)

    GraphColor.color[FieldSymbol, RegisterOffset](nodes, neighbors, colors)
  }

}
