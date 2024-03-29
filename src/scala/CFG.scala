package compile

// A CFG is a digraph where the nodes are IR2.Blocks and the edges are IR2.Transitions.
// There is a single entry point and a single exit point, marked as start and end.
// So a control flow of a -> b -> c may look like (pseudocode)
//   CFG(a, c, {a -> Edge(b), b -> Ed)ge(c)})
// We hold on to the end so that CFGs can be easily chained/combined
class CFG(val start: IR2.Block, val end: IR2.Block, val edges: IR2.EdgeMap) {
  import IR2._

  /* All blocks reachable from start */
  lazy val blocks:Set[Block] = findBlocks()

  lazy val dominates:Map[Block, Set[Block]] = findDominators()
  
  /* Looks up and returns the predecessors of `block` */
  def predecessors(block:Block):Set[Block] = reverseEdges get block match {
    case Some(s:Set[Block]) => s
    case None => Set[Block]()
  }

  class CFGIntegrityError(msg: String) extends RuntimeException(msg)

  /* For each block, the list of predecessors */
  lazy val reverseEdges:Map[Block, Set[Block]] = blocks.map( b => (b, reverseEdgesForBlock(b)) ).toMap

  /* Find predecessors of `block` 
   * helper for reverseEdges */
  private def reverseEdgesForBlock(block:Block):Set[Block] = (edges.keys filter { pred =>
    edges.get(pred) match {
      case None => false
      case Some(Edge(next)) => next == block
      case Some(Fork(_, left, right)) => (left == block) || (right == block)
    }
  }).toSet

  /* Chains CFGs by attaching `rhs` to the end of `this`
   * Requires that there are no blocks in both CFGs 
   */
  def ++(rhs: CFG): CFG = {
    val newEdges = (edges ++ rhs.edges) + (end -> Edge(rhs.start))
    new CFG(start, rhs.end, newEdges)
  }

  def +(block: Block): CFG = {
    val newEdges = edges + (end -> Edge(block))
    new CFG(start, block, newEdges)
  }

  private def findBlocks(): Set[Block] = {
    var collector = Set[Block]()
    collector += start
    collector += end
    edges.foreach{
      case (a, Edge(b)) =>
        collector += a
        collector += b
      case (a, Fork(_, b, c)) =>
        collector += a
        collector += b
        collector += c
    }
    assert(collector.contains(start), "start not in blocks")
    assert(collector.contains(end), "end not in blocks")
    return collector
  }

  private def findDominators(): Map[Block, Set[Block]] = {
    val initialMap:Map[Block, Set[Block]] = findBlocks.map{ block:Block =>
      edges.get(block) match {
        case None => (block -> Set[Block]())
        case Some(Edge(next)) => (block -> Set(next))
        case Some(Fork(_, l, r)) => (block -> Set(l, r))
      }
    }.toMap
    
    def applyTransitivity(partialMap:Map[Block, Set[Block]]) = partialMap map {
      case (dominator, dominatees) =>
        val newDominatees:Set[Block] = dominatees.flatMap(block => partialMap(block))
        (dominator, dominatees union newDominatees - dominator)
    }

    def iterate(partialMap:Map[Block, Set[Block]]): Map[Block, Set[Block]] = {
      val newMap = applyTransitivity(partialMap)
      if (partialMap == newMap) newMap
      else iterate(newMap)
    }
    
    iterate(initialMap)
  }

  /* Condense every pair of blocks a->b in `this` where a has one edge out
   * and b has one edge in. */
  def condense(): CFG = {
    // Warning: this is not very fast and has lots of var's
    // Blocks whose sucessor(s) must be a new block
    // i.e. blocks who cannot collapse with their successor(s)
    var condensed:Set[Block] = Set()

    var currentCFG:CFG = this

    // More like *potentially* condensable.
    var condensable:Set[Block] = currentCFG.blocks

    // we can do more!
    while (!condensable.isEmpty) {
      // Just to get an arbitrary-ish block
      val block = condensable.head

      // condense block as far as possible
      val (newCFG, newBlock) = currentCFG.condenseFromBlock(block)

      // Ok starting from `block` we have condensed fully
      condensed = condensed + newBlock
      condensable = newCFG.blocks -- condensed
      currentCFG = newCFG
    }
    // we're done
    currentCFG
  }

  /* Attempt to condense `block` and its successor repeatedly.
   * If `block` has >1 successor, return `this`
   * If `block`'s successor has >1 edge in, return `this`
   * Otherwise combine them and return a new CFG
   * Keep condensing until you can't
   *
   * helper for `condense`
   */
  private def condenseFromBlock(b: Block): Tuple2[CFG, Block] = {
    var block = b
    var newCFG = this
    while(true) {
      newCFG.edges get block match {
        case Some(Edge(next)) => newCFG.reverseEdges(next).size match {
          case 1 => // condense `block` and `next`
            // Constraint: `block` and `next` have the same symbol table
            val newBlock = Block(block.stmts ++ next.stmts, block.loopHead || next.loopHead)
            val newStartBlock = (newCFG.start == block) match {
              case true => newBlock
              case _ => newCFG.start
            }
            val newEndBlock = (newCFG.end == next) match {
              case true => newBlock
              case _ => newCFG.end
            }
            // My predecessors need to be reassigned by reassignEdges
            val newInEdges = reassignInEdges(block, newBlock, newCFG)
            val newEdges = reassignOutEdges(next, newBlock, newInEdges) - block
            newCFG = new CFG(newStartBlock, newEndBlock, newEdges)
            block = newBlock
          case _ =>  // Next has >1 edge in
            return (newCFG, block)
        }
        case _ => // Some(Fork) or None 
          return (newCFG, block)
      }
    }
    // Not reachable
    (newCFG, block)
  }

  /* Helper for `condenseFromBlock`
   * Returns an edge map based on the current `edges` but edges in to `oldBlock`
   * become edges into `newBlock`.
   */
  private def reassignInEdges(oldBlock:Block, newBlock:Block, cfg:CFG):Map[Block, Transition] = {
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

  /*
   * Inserts a CFG in place of the given block.
   * All edges that would have lead to the original block now lead to the inserted CFG.
   * All edges that would have come out of the original block now lead out of the inserted CFG.
   * The original block will no longer exist.
   */
  def replaceBlock(block: Block, inlined:CFG) : CFG = {
    var newEdges = this.edges ++ inlined.edges
    // Replace all edges leading IN to the original block with edges going to the inlined cfg's start
    newEdges = newEdges.mapValues {
      case Edge(to) => if (to == block) Edge(inlined.start) else Edge(to)
      case Fork(cond, left, right) => if (left == block && right == block) {
        Fork(cond, inlined.start, inlined.start)
      } else if (left == block) {
        Fork(cond, inlined.start, right)
      } else if (right == block) {
        Fork(cond, left, inlined.start)
      } else {
        Fork(cond, left, right)
      }
    }
    // Replace the edge leading OUT of the original block with an edge coming out of the inlined cfg's end
    newEdges = newEdges.map {case (b, e) => {
      if (b == block) (inlined.end, e) else (b, e)
    }}
    val newStart = if (this.start == block) inlined.start else this.start
    val newEnd = if (this.end == block) inlined.end else this.end
    new CFG(newStart, newEnd, newEdges)
  }

  /* Apply a transform to all blocks and return a new CFG */
  def mapBlocks(func: Block => Block): CFG = {
    // Translation from old blocks to new blocks
    val bTrans:Map[Block, Block] = blocks.map{ b =>
      b -> func(b)
    }.toMap

    val newEdges:Map[Block, Transition] = edges.keys.map{ oldBlock  =>
      val target = edges(oldBlock) match {
        case Edge(oldDest) => Edge(bTrans(oldDest))
        case Fork(c, oldTrue, oldFalse) => Fork(c, bTrans(oldTrue), bTrans(oldFalse))
      }
      bTrans(oldBlock) -> target
    }.toMap

    new CFG(bTrans(start), bTrans(end), newEdges)
  }

  def flatMapBlocks(func: Block => List[Block]): CFG = {
    val bTrans:Map[Block, CFG] = blocks.map { b =>
      val cfgs = func(b).map(CFG.fromBlock)
      b -> CFG.chain(cfgs)
    }.toMap
    var newCFG = this
    bTrans.foreach {case (b, cfg) =>
      newCFG = newCFG.replaceBlock(b, cfg)
    }
    return newCFG
  }

  override def toString: String = {
    "CFG(\n  %s)".format(
      blocks.map(_.toString.split('\n').mkString("\n  ")).mkString("\n  "))
  }

  /*
  private def validate() = {
    //print("Validating CFG")
    mustReach(end)
    edges.keys.foreach(mustReach)
    edges.values.foreach{ _ match {
      case Edge(next) => mustReach(next)
      case Fork(_, left, right) => mustReach(left); mustReach(right)
    }}
    //print("Finished validating")
  }
  */

  /*
  private def mustReach(block: Block): Unit = {
    // TODO maybe traversing all the time is slow.
    traverse(start).contains(block) match {
      case true =>
      case false => throw new CFGIntegrityError(s"block not reachable $block")
    }
  }
  */
}

object CFG {
  import IR2._

  def fromStatement(stmt: IR2.Statement): CFG = {
    val block = Block(List(stmt))
    new CFG(block, block, Map[IR2.Block, IR2.Transition]())
  }

  def fromBlock(block: IR2.Block): CFG = {
    new CFG(block, block, Map())
  }


  def nopBlock(loopHead:Boolean = false): Block = IR2.Block(List(), loopHead)

  def dummy(): CFG = {
    val b = nopBlock()
    new CFG(b, b, Map[IR2.Block, IR2.Transition]())
  }

  def chain(cfgs: TraversableOnce[CFG]): CFG =
    cfgs.fold(dummy)(_ ++ _)
}
