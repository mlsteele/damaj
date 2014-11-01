package compile

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
abstract trait Analysis {
  import IR2._
  /**
    * The type of the internal state.  This is usually a set of some
    * kind. For example, for a Reaching Statements analysis, T would
    * be defined as type T = Set[Statement], representing the
    * statements which actually reach a certain program point.
    */
  type T; // The datatype of the internal state, usually a set

  /*
   * The member representing nothing, a default state.  For any
   * set-based Analysis, this is most likely going to be the empty
   * set.
   */
  def bottom() : T

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
  final def analyze(method: Method) : Map[Block, T] = {
    var state:Map[Block, T] = Map()

    // In forward, we choose start as the first block, otherwise choose the end block
    val firstBlock = direction() match {
      case Forward  => method.cfg.start
      case Backward => method.cfg.end
    }

    // Process the first block, which has no input
    val firstOutput:T = transfer(bottom(), firstBlock)
    state = state + ((firstBlock, firstOutput))

    // In the forward direction, the next nodes to process will be the successors
    val next:(Block => Set[Block]) = direction() match {
      case Forward  => successors(method.cfg, _)
      case Backward => predecessors(method.cfg, _)
    }

    val prev:(Block => Set[Block]) = direction() match {
      case Forward  => predecessors(method.cfg, _)
      case Backward => successors(method.cfg, _)
    }

    var workSet: Set[Block] = next(firstBlock)

    while (!workSet.isEmpty) {
      // Remove from the work set
      val block = workSet.head
      workSet = workSet - block
      // Calculate the inputs from all the nodes brancing to this one
      val inputs: Set[T] = prev(block).map(state(_))
      val combinedInput: T = inputs.reduce(merge _)
      // Retrieve the previous value of the transfer function
      val prevOutput: T = state.getOrElse(block, bottom())
      // Calculate new value
      val newOutput = transfer(combinedInput, block)
      if (newOutput != prevOutput) {
        // If the value has changed, update state, and add successors to working set
        state = state + ((block, newOutput))
        workSet = workSet ++ next(block)
      }
    }

    return state
  }

  private def successors(cfg: CFG, block: Block) : Set[Block] = cfg.edges(block) match {
    case None => Set()
    case Some(Edge(to)) => Set(to)
    case Some(Fork(_, left, right)) => Set(left, right)
  }

  private def predecessors(cfg: CFG, block: Block) : Set[Block] = cfg.edges.keys.filter {k =>
    cfg.edges(k) match {
      case None => false
      case Some(Edge(next)) => next == block
      case Some(Fork(_, left, right)) => (left == block) || (right == block)
    }
  }
}
