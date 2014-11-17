package compile
import collection.mutable.Stack
import org.scalatest._

class IR2Spec extends FlatSpec with Matchers {
  import IR2._

  def exampleStmt: Statement = Return(Some(LoadLiteral(0)))

  "Block" should "be equal to itself" in {
    val a = Block(List(exampleStmt))
    (a == a) shouldBe true
  }

  it should "not be equal to a block with the same statements" in {
    val a = Block(List(exampleStmt))
    val b = Block(List(exampleStmt))
    (a == b) shouldBe false
  }
}
