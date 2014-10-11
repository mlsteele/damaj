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
  case class Equals()           extends RelationalBinOp
  case class NotEquals()        extends RelationalBinOp

  sealed trait BooleanBinOp extends BinOpType
  case class And()              extends BooleanBinOp
  case class Or()               extends BooleanBinOp

  sealed trait UnaryOpType
  case class Not()              extends UnaryOpType
  case class Negative()         extends UnaryOpType

  implicit class EnhancedBinOpType (op: BinOpType) {
    /**
      * Returns the type of the operand.  Returns None if the operand
      * type needs context to resolve (for example, in the case of a
      * ==, the operands could be bool or int.
      */
    def operandType() : Option[DType] = op match {
      case _:ArithmeticBinOp => Some(DTInt)
      case _:BooleanBinOp    => Some(DTBool)
      case _:RelationalBinOp => None
      }

    def returnType() : DType = op match {
      case _:ArithmeticBinOp => DTInt
      case _:BooleanBinOp    => DTBool
      case _:RelationalBinOp => DTBool
    }
  }

  implicit class EnhancedUnaryOp (op: UnaryOpType) {
    def operandType(): DType = op match {
      case _:Not      => DTInt
      case _:Negative => DTBool
    }

    def returnType() : DType = operandType()
  }
}
