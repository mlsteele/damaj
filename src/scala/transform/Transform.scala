package compile

trait Transformation {
  import IR2._

  def transform(ir: Program): Program =
    Program(
      ir.fields,
      transform(ir.main),
      ir.methods.map(transform))

  def transform(m: Method): Method =
    Method(m.id,
      m.params,
      m.symbols,
      transform(m.cfg),
      m.returnType)

  def transform(cfg: CFG): CFG
}
