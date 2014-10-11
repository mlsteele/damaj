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

  sealed trait BinOpType
  sealed trait ArithmeticBinOp extends BinOpType
  case class Add()              extends ArithmeticBinOp
  case class Subtract()         extends ArithmeticBinOp
  case class Multiply()         extends ArithmeticBinOp
  case class Divide()           extends ArithmeticBinOp
  case class Mod()              extends ArithmeticBinOp

  sealed trait RelationalBinOp extends BinOpType
  case class LessThan()         extends RelationalBinOp
  case class GreaterThan()      extends RelationalBinOp
  case class LessThanEqual()    extends RelationalBinOp
  case class GreaterThanEqual() extends RelationalBinOp

  sealed trait EqualityBinOp extends BinOpType
  case class Equals()           extends EqualityBinOp
  case class NotEquals()        extends EqualityBinOp

  sealed trait BooleanBinOp extends BinOpType
  case class And()              extends BooleanBinOp
  case class Or()               extends BooleanBinOp

  sealed trait UnaryOpType
  case class Not()              extends UnaryOpType
  case class Negative()         extends UnaryOpType
  case class Length()           extends UnaryOpType

  val stringToBinOpMap : Map[String, BinOpType] =
    Map(
      "+" ->  Add(),
      "-" -> Subtract(),
      "*" -> Multiply(),
      "/" -> Divide(),
      "%" -> Mod(),
      "<" -> LessThan(),
      ">" -> GreaterThan(),
      "<=" -> LessThanEqual(),
      ">=" -> GreaterThanEqual(),
      "==" -> Equals(),
      "!=" -> NotEquals(),
      "&&" -> And(),
      "||" -> Or()
    )

  val binToStringOpMap : Map[BinOpType, String] = stringToBinOpMap.map(_.swap)

  val stringToUnaryOpMap : Map[String, UnaryOpType] = 
    Map(
      "-" -> Negative(),
      "!" -> Not(),
      "@" -> Length()
    )

  val unaryOpToStringMap : Map[UnaryOpType, String] = stringToUnaryOpMap.map(_.swap)

  // Converts strings to bin ops
  implicit def StringToBinOp(s: String) : BinOpType = stringToBinOpMap get s match {
    case None => assert(false, "Failed to convert string to bin op. This shouldn't happen."); Add()
    case Some(b) => b
  }

  // Implicit convert bin ops to strings
  implicit def BinOpToString(b: BinOpType) : String = binToStringOpMap get b match {
    case None => assert(false, "Failed to convert bin op to string. This shouldn't happen."); "WTF"
    case Some(s) => s
  }

  // Converts strings to unary ops
  implicit def StringToUnaryOp(s: String) : UnaryOpType = stringToUnaryOpMap get s match {
    case None => assert(false, "Failed to convert string to unary op. This shouldn't happen."); Not()
    case Some(u) => u
  }

  // Implicitly convert a unary op to a string
  implicit def UnaryOpToString(u: UnaryOpType) : String = unaryOpToStringMap get u match {
    case None => assert(false, "Failed to convert unary to string. This shouldn't happen."); "WTF"
    case Some(s) => s
  }

  implicit class EnhancedBinOpType (op: BinOpType) {
    /**
      * Returns the type of the operand.  Returns None if the operand
      * type needs context to resolve (for example, in the case of a
      * ==, the operands could be bool or int.
      */
    def operandType() : Option[DType] = op match {
      case _:ArithmeticBinOp => Some(DTInt)
      case _:BooleanBinOp    => Some(DTBool)
      case _:RelationalBinOp => Some(DTInt)
      case _:EqualityBinOp   => None
      }

    def returnType() : DType = op match {
      case _:ArithmeticBinOp => DTInt
      case _:BooleanBinOp    => DTBool
      case _:RelationalBinOp => DTBool
      case _:EqualityBinOp   => DTBool
    }
  }

  implicit class EnhancedUnaryOp (op: UnaryOpType) {
    def operandType(): DType = op match {
      case _:Not      => DTInt
      case _:Negative => DTBool
      case _:Length   => DTInt
    }

    def returnType() : DType = operandType()
  }
}
