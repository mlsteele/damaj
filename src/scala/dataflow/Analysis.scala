package compile

object Analysis {
  import IR2._
  case class AnalysisResult[T](outputs: Map[Block, T], inputs: Map[Block, T])
}

/**
  * Abstract class reprenting a dataflow analysis.  To implement an
  * analysis, you must define the following methods:
  * - bottom
  * - direction
  * - merge
  * - transfer
  * 
  * You must also define the T type, which represents the internal
  * state.
  */
trait Analysis {
  import Analysis._
  import IR2._
  /**
    * The type of the internal state.  This is usually a set of some
    * kind. For example, for a Reaching Statements analysis, T would
    * be defined as type T = Set[Statement], representing the
    * statements which actually reach a certain program point.
    */
  type T // The datatype of the internal state, usually a set

  val method: Method
  val cfg: CFG = method.cfg

  /*
   * The member representing nothing, a default state.  For any
   * set-based Analysis, this is most likely going to be the empty
   * set.
   */
  def bottom() : T


  /**
    * The value used as the first block's input.
    */
  def initial(): T

  /**
    * The direction of the analysis must be specified as either
    * forward or backwards.
    */
  abstract sealed trait AnalysisDirection;
  case object Forward  extends AnalysisDirection;
  case object Backward extends AnalysisDirection;
  def direction() : AnalysisDirection

  /*
   * The merging operator for analysis results from each path
   */
  def merge(a:T, b:T) : T

  /*
   * The transfer function. This takes as input the previous state,
   * the current statement, and returns a new state.
   */
  def transfer(previous: T, block: Block) : T

  /*
   * Runs the dataflow analysis using the overriden
   * operations. Returns the information known about each statement.
   * If direction() is Forward, then each value in the map is to be
   * interpreted as the information known AFTER the block. If
   * direction() is Backward, then each value in the map is to be
   * interpreted as the information known BEFORE the block.
   */
  final def analyze() : AnalysisResult[T] = {
    val allBlocks:Set[Block] = cfg.blocks

    // Initialize all outputs to be f(⊥)
    var outputs:Map[Block, T] = allBlocks.map {b:Block =>
      (b -> transfer(bottom(), b))
    }.toMap
    // Initialize all inputs to be IDKLOL
    var inputs:Map[Block, T] = Map()

    // In forward, we choose start as the first block, otherwise choose the end block
    val firstBlock = direction() match {
      case Forward  => cfg.start
      case Backward => cfg.end
    }

    // Use initial value as first block's input
    inputs += (firstBlock -> initial())
    outputs += (firstBlock -> transfer(inputs(firstBlock), firstBlock))

    // In the forward direction, the next nodes to process will be the successors
    val next:(Block => Set[Block]) = direction() match {
      case Forward  => successors(cfg, _)
      case Backward => predecessors(cfg, _)
    }

    val prev:(Block => Set[Block]) = direction() match {
      case Forward  => predecessors(cfg, _)
      case Backward => successors(cfg, _)
    }

    var workSet: Set[Block] = allBlocks - firstBlock

    while (!workSet.isEmpty) {
      // Remove from the work set
      val block = workSet.head
      workSet -= block
      // Calculate the inputs from all the nodes brancing to this one
      val blockInputs: Set[T] = prev(block).map(outputs(_))
      val combinedInput: T = blockInputs.fold(bottom)(merge _)
      inputs += (block -> combinedInput)
      // Retrieve the previous value of the transfer function
      val prevOutput: T = outputs(block)
      // Calculate new value
      val newOutput = transfer(inputs(block), block)
      if (newOutput != prevOutput) {
        // If the value has changed, update outputs, and add successors to working set
        outputs += (block -> newOutput)
        workSet ++= next(block)
      }
    }

    return AnalysisResult[T](outputs, inputs)
  }

  private def successors(cfg: CFG, block: Block) : Set[Block] = cfg.edges.get(block) match {
    case None => Set()
    case Some(Edge(to)) => Set(to)
    case Some(Fork(_, left, right)) => Set(left, right)
  }

  private def predecessors(cfg: CFG, block: Block) : Set[Block] = cfg.predecessors(block)
}
