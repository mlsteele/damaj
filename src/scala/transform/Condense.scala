package compile

object Condense extends Transformation {
  override def transform(cfg: CFG): CFG = cfg.condense()
}
