package compile

/**
  * Inserts code to detect falloff detection for methods that are supposed to return a value.
  */
object FalloffDetection {
  import IR2._
  import IRShared._

  def apply(program: Program): Program = {
    program.copy(
      methods = program.methods.map {
        case m@Method(id, params, locals, cfg, DTVoid) => m
        case m:Method => insertFalloffDetection(m)
      },
      main = program.main.returnType match {
        case IRShared.DTVoid => program.main
        case _ => insertFalloffDetection(program.main)
      }
    )
  }

  private def insertFalloffDetection(method: Method) : Method = {
    val errorBlock = List(
      Call("printf", List(Left(StrLiteral("*** RUNTIME ERROR ***: Control fell off non-void method")))),
      Call("exit",   List(Right(LoadLiteral(-2))))
    )
    val realEnd = CFG.nopBlock()
    method.copy(
      cfg = method.cfg ++ CFG.fromBlock(errorBlock) ++ CFG.fromBlock(realEnd)
    )
  }
}
