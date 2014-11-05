package compile


object IR2 {
  import IRShared._
  import SymbolTable._

  // Earlier version constrain all fields to come before methods.
  // The callouts are discarded.
  case class Program(fields: List[Field], main: Method, methods: List[Method]) {
    override def toString() = "%s\n%s\n%s".format(
      fields.mkString("\n"),
      main.toString,
      methods.mkString("\n\n")
    )
  }

  case class Field(id: ID, size: Option[Long]) {
    override def toString() = "%s%s".format(id,
      size match {
        case Some(l:Long) => "[" + l + "]"
        case None => ""
      }
    )
  }

  case class Method(id: ID, params: List[Field], symbols: SymbolTable, cfg: CFG, returnType:DType ) {
    override def toString() = "%s %s(%s) {\n  %s\n  %s}".format(returnType, id,
      params.mkString(", "),
      symbols.toString,
      cfg.toString.split('\n').mkString("\n  "))
  }

  // Not the same as an IR block! Cannot contain any control flow statements.
  // The lecture notes from 10/9 explain the idea.
  case class Block(stmts: List[Statement], fields: SymbolTable) {
    val uuid = SlugGenerator.id(this)
    override def equals(that:Any):Boolean = that match {
      case that: Block => this eq that
      case _ => false
    }
    override def toString() = "Block %s:\n  %s".format(uuid,
      stmts.map(_.toString.split('\n').mkString("\n  ")).mkString("\n  "))
  }

  // in IR2, Statements can not contain control flow.
  sealed trait Statement
  case class Assignment(left: Store, right: Expr) extends Statement {
    override def toString = "%s = %s".format(left, right)
  }
  // TODO call should Load
  case class Call(id: ID, args:List[Either[StrLiteral, Expr]]) extends Statement with Expr {
    override def toString = {
      val argStrings = args.map{ _ match {
        case Left(StrLiteral(str)) => Escape.escape(Escape.escape(str))
        case Right(expr) => expr
      }}
      "%s(%s)".format(id, argStrings.mkString(", "))
    }
  }

  case class Return(value: Option[Load]) extends Statement {
    override def toString = value match {
      case Some(value) => s"return $value"
      case None => "return"
    }
  }

  sealed trait Expr
  case class BinOp(left: Load, op: BinOpType, right: Load) extends Expr {
    override def toString = "%s %s %s".format(left, op, right)
  }
  case class UnaryOp(op: UnaryOpType, right: Load) extends Expr {
    override def toString = "%s %s".format(op, right)
  }

  sealed trait Load extends Expr
  // hmm, should this have something other than a FieldSymbol?
  case class LoadField(from: FieldSymbol, index: Option[Load]) extends Load {
    override def toString = "%s%s".format(from.id, index match {
      case Some(i:Load) => "[" + i + "]"
      case None => ""
    })
  }
  case class LoadLiteral(value: Long) extends Load {
    override def toString = value.toString
  }

  case class Store(to: FieldSymbol, index: Option[Load]) {
    override def toString = "%s%s".format(to.id, index match {
      case Some(i:Load) => i
      case None => ""
    })
  }

  sealed trait Transition
  case class Edge(to: Block) extends Transition {
    override def toString() = "Edge to %s".format(to.uuid)
  }
  case class Fork(condition: Load, ifTrue: Block, ifFalse: Block) extends Transition {
    override def toString() = "Fork condition: %s\nTrue jump: %s\nFalse jump: %s".format(
      condition, ifTrue, ifFalse)
  }

  type EdgeMap = Map[IR2.Block, IR2.Transition]
}

class IR2Printer(ir2: IR2.Program) {
  import IR2._

  val print: String = printIR2(ir2)

  private def printIR2(ir2: Program): String =
    "IR2.Program(%s)".format(ir2)
    //"IR2(... pretty sweet ir2, wish I knew how to print it ...)"
}

// A CFG is a digraph where the nodes are IR2.Blocks and the edges are IR2.Transitions.
// There is a single entry point and a single exit point, marked as start and end.
// So a control flow of a -> b -> c may look like (pseudocode)
//   CFG(a, c, {a -> Edge(b), b -> Ed)ge(c)})
// We hold on to the end so that CFGs can be easily chained/combined
class CFG(val start: IR2.Block, val end: IR2.Block, val edges: IR2.EdgeMap) {
  import IR2._

  /* All blocks reachable from start */
  private var blocks:Option[Set[Block]] = None
  
  def getBlocks():Set[Block] = blocks match {
    case None => 
      blocks = Some(traverse(start, Set[Block]()))
      blocks.get
    case Some(s:Set[Block]) => s
  }

  /* Looks up and returns the predecessors of `block` */
  def predecessors(block:Block):Set[Block] = reverseEdges get block match {
    case Some(s:Set[Block]) => s
    case None => Set[Block]()
  }

  class CFGIntegrityError(msg: String) extends RuntimeException(msg)

  /* For each block, the list of predecessors */
  lazy val reverseEdges:Map[Block, Set[Block]] = getBlocks.map( b => (b, reverseEdgesForBlock(b)) ).toMap

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

  /* Find all the blocks 
   * helper for `blocks` and `toString` */
  private def traverse(from: Block, traversed:Set[Block]): Set[Block] = {
    if (traversed contains from) return Set()
    val newTraversed = traversed + from

    val rest: Set[Block] = edges get from match {
      case None => Set()
      case Some(Edge(next)) => traverse(next, newTraversed)
      case Some(Fork(_, left, right)) => traverse(left, newTraversed) union traverse(right, newTraversed)
    }
    Set(from) union rest
  }

  /* Condense every pair of blocks a->b in `this` where a has one edge out
   * and b has one edge in. */
  def condense(): CFG = {
    // Warning: this is not very fast and has lots of var's
    println("Starting condensing")
    // Blocks whose sucessor(s) must be a new block
    // i.e. blocks who cannot collapse with their successor(s)
    var condensed:Set[Block] = Set()

    var currentCFG:CFG = this

    // More like *potentially* condensable.
    var condensable:Set[Block] = currentCFG.getBlocks

    // we can do more!
    while (!condensable.isEmpty) {
      println("%d things are condensable".format(condensable.size))
      // Just to get an arbitrary-ish block
      val block = condensable.head

      // condense block as far as possible
      val (newCFG, newBlock) = currentCFG.condenseFromBlock(block)
      //while (newCFG.getBlocks.size != currentCFG.getBlocks.size) {
        //println("new CFG size: %d".format(newCFG.getBlocks.size))
        //println("current CFG size: %d".format(currentCFG.getBlocks.size))
        //// fixed point woo
        //currentCFG = newCFG
        //newCFG = newCFG.condenseFromBlock(block)
      //}
      

      // Ok starting from `block` we have condensed fully
      condensed = condensed + newBlock
      condensable = newCFG.getBlocks -- condensed
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
  private def condenseFromBlock(b:Block):Tuple2[CFG, Block] = {
    var block = b
    var newCFG = this
    println("Start CFG has %d nodes".format(newCFG.getBlocks.size))
    while(true) {
      println("Condensing from " + block.uuid)
      println("newCFG has blocks: " + newCFG.getBlocks.map(_.uuid).mkString(", "))
      println("newCFG has reverseBlocks: " + newCFG.reverseEdges.keys.map(_.uuid).mkString(", "))
      newCFG.edges get block match {
        case Some(Edge(next)) => newCFG.reverseEdges(next).size match {
          case 1 => // condense `block` and `next`
            // Constraint: `block` and `next` have the same symbol table
            println("It has a condensable next block, " + next.uuid)
            val newBlock = Block(block.stmts ++ next.stmts,  block.fields)
            println("Made a new block " + newBlock.uuid)
            val newStartBlock = (newCFG.start == block) match {
              case true => newBlock
              case _ => newCFG.start
            }
            // My predecessors need to be reassigned by reassignEdges
            val newInEdges = reassignInEdges(block, newBlock, newCFG)
            val newEdges = reassignOutEdges(next, newBlock, newInEdges) - block
            newCFG = new CFG(newStartBlock, end, newEdges)
            println("newCFG has %d nodes".format(newCFG.getBlocks.size))
            block = newBlock
          case _ => 
            println("Next has more than one edge in")
            return (newCFG, block)
        }
        case _ => 
          println("It doesn't have a plain edge out")
          return (newCFG, block)
      }
    }
    (newCFG, block)
  }

  /* Helper for `condenseFromBlock`
   * Returns an edge map based on the current `edges` but edges in to `oldBlock`
   * become edges into `newBlock`.
   */
  private def reassignInEdges(oldBlock:Block, newBlock:Block, cfg:CFG):Map[Block, Transition] = {
    val edgesToRemove:Set[Block] = cfg.reverseEdges(oldBlock)
    println("There are %d edges to change from %s to %s".format(edgesToRemove.size, oldBlock.uuid, newBlock.uuid))
    val edgesToAdd:Map[Block, Transition] = edgesToRemove.map( from => cfg.edges(from) match {
        case Edge(o) => 
          println("Translating an edge(" + o.uuid + ")")
          from -> Edge(newBlock)
        case Fork(c, l, r) =>
          println("Translating a Fork(" + l.uuid + ", " + r.uuid + ")")
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
        println("No out edge from %s to reassign".format(oldBlock.uuid))
        edgemap
      case Some(t) => 
        println("Reassigning an out edge from %s to %s".format(oldBlock.uuid, newBlock.uuid))
        (edgemap - oldBlock) + (newBlock -> t)
    }
  }

  /* Apply a transform to all blocks and return a new CFG */
  def mapBlocks(func: Block => Block): CFG = {
    // Translation from old blocks to new blocks
    val bTrans:Map[Block, Block] = getBlocks.map{b => (b, func(b))}.toMap
    val newEdges:Map[Block, Transition] = edges.keys.map{oldBlock  =>
      bTrans(oldBlock) -> (edges(oldBlock) match {
        case Edge(oldDest) => Edge(bTrans(oldDest))
        case Fork(c, oldTrue, oldFalse) => Fork(c, bTrans(oldTrue), bTrans(oldFalse))
      }
    )}.toMap
    new CFG(bTrans(start), bTrans(end), newEdges)
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

object CFGFactory {
  import IR2._
  import SymbolTable._

  def fromStatement(stmt: IR2.Statement, fields: SymbolTable): CFG = {
    val block = Block(List(stmt), fields)
    new CFG(block, block, Map[IR2.Block, IR2.Transition]())
  }

  def nopBlock: Block = IR2.Block(List(), new SymbolTable())
  def dummy: CFG = {
    val b = nopBlock
    new CFG(b, b, Map[IR2.Block, IR2.Transition]())
  }

  def chain(cfgs: TraversableOnce[CFG]): CFG =
    cfgs.fold(dummy)(_ ++ _)
}
