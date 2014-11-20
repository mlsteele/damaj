package compile

// Removes empty blocks from the CFG
object PruneCFG extends Transformation {
  override def transform(cfg: CFG) : CFG = {
    cfg
  }
}
