abstract trait Analysis {
  type T;

  /*
   * The merging operator for sets from each path.
   */
  def confluenceOperator(a:Set[T], b: Set[T]) : Set[T]
}
