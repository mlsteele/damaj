package compile

/*
 * Useless example of a transformation.
 * Can be used to make sure Transformation and mapBlocks are working.
 * Adds `return 9` to the end of every block.
 */
object AddReturnsEverywhere extends Transformation {
  import IR2._

  override def transform(cfg: CFG): CFG =
    cfg.mapBlocks{ b =>
      Block(b.stmts :+ Return(Some(LoadLiteral(9))))
    }
}
