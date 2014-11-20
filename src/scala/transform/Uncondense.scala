package compile

// Takes any blocks with multiple statements and splits them into multiple blocks
object Uncondense extends Transformation {
  import IR2._

  override def transform(method: Method) : Method = {
    var cfg = method.cfg
    for (block <- cfg.blocks) {
      // Check if the block multiple statements
      val stmts = block.stmts
      (stmts.length > 1) match {
        case true => {
          // Spit up statements into a list of single-statement blocks
          val newBlocks:List[CFG] = block.stmts.map{s => CFG.fromBlock(Block(List(s)))}
          // Replace this block with the list-of-statements-cfg
          cfg = cfg.replaceBlock(block, CFG.chain(newBlocks))
        }
        case _ =>
      }
    }
    return method.copy(
      cfg=cfg
    )
  }
}
