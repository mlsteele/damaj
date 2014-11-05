package compile

object Condense extends Transformation {
  def transform(cfg: CFG): CFG = cfg.condense()
}
