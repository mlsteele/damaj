package compile

// Takes any blocks with multiple statements and splits them into multiple blocks
object Uncondense extends Transformation {
  import IR2._

  override def transform(method: Method) : Method = {
    val cfg = method.cfg
    for (block <- cfg.blocks) {
      // Check if the block multiple statements
      block.stmts match {
        case (first :: rest) if (rest.length >= 1) => {
          val firstBlock = Block(List(first))
          val secondBlock = Block(rest)

          val newInEdges:EdgeMap = reassignInEdges(block, firstBlock, cfg)
          val newOutEdges:EdgeMap = reassignOutEdges(block, secondBlock, newInEdges)

          // Remove old block from edge map
          val newEdges = (newInEdges ++ newOutEdges - block) +
            (firstBlock -> Edge(secondBlock)) 

          // If the start or end blocks were replaced, make sure to deal with it
          val newCFG = new CFG(
            if (block == cfg.start) firstBlock else cfg.start,
            if (block == cfg.end) secondBlock else cfg.end,
            newEdges)
          val newMethod = Method(
            method.id,
            method.params,
            method.locals,
            newCFG,
            method.returnType
          )
          return transform(newMethod)
        }
        case _ =>
      }
    }
    return method
  }

  private def reassignInEdges(oldBlock:Block, newBlock:Block, cfg:CFG) : Map[Block, Transition] = {
    val edgesToRemove:Set[Block] = cfg.reverseEdges(oldBlock)
    val edgesToAdd:Map[Block, Transition] = edgesToRemove.map( from => cfg.edges(from) match {
        case Edge(o) => 
          from -> Edge(newBlock)
        case Fork(c, l, r) =>
          if (l == oldBlock) {
            from -> Fork(c, newBlock, r)
          } else {
            from -> Fork(c, l, newBlock)
          }
      }
    ).toMap
    (cfg.edges -- edgesToRemove) ++ edgesToAdd
  }

  private def reassignOutEdges(oldBlock:Block, newBlock:Block, edgemap:Map[Block, Transition]):Map[Block, Transition] = {
    edgemap.get(oldBlock) match {
      case None => 
        edgemap
      case Some(t) => 
        (edgemap - oldBlock) + (newBlock -> t)
    }
  }
}
