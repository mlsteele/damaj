package compile

/**
  * Abstract class reprenting a dataflow analysis.  To implement an
  * analysis, you must define the following methods:
  * - bottom
  * - direction
  * - merge
  * - transfer
  */
abstract trait Analysis {
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
  case class Forward()  extends AnalysisDirection;
  case class Backward() extends AnalysisDirection;
  def direction() : AnalysisDirection

  /*
   * The merging operator for analysis results from each path
   */
  def merge(a:T, b:T) : T

  /*
   * The transfer function. This takes as input the previous state,
   * the current statement, and returns a new state.
   */
  def transfer(previous: T, stmt: IR2.Statement) : T

  /*
   * Runs the dataflow analysis using the overriden
   * operations. Returns the information known about each statement.
   */
  final def analyze(method: IR2.Method) : Map[IR2.Statement, T] = {
    // TODO: actually run the dataflow algorithm
    return Map()
  }
}
