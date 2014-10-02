package compile

// Leaf types used in multiple IRs including the AST
object IRShared {
  type ID = String

  sealed trait DType
  case object DTVoid extends DType
  case object DTInt extends DType
  case object DTBool extends DType

  sealed trait CommonLiteral
  case class IntLiteral(value: BigInt) extends CommonLiteral
  case class CharLiteral(value: Char) extends CommonLiteral
  case class BoolLiteral(value: Boolean) extends CommonLiteral
  // String literal is not a literal! It's only used in callout args.
  case class StrLiteral(value: String)
}
