package compile
import org.scalatest._

class CFGSpec extends FlatSpec with Matchers {
  import IR2._

  "Dominates map" should "maintain transitivity" in {
    val a = Block(List(Return(Some(LoadLiteral(1)))))
    val b = Block(List(Return(Some(LoadLiteral(2)))))
    val c = Block(List(Return(Some(LoadLiteral(3)))))
    val edges = Map((a -> Edge(b)), (b -> Edge(c)))
    val cfg = new CFG(a, c, edges)
    val expectedDominates = Map((a -> Set(b, c)), (b -> Set(c)), (c -> Set[Block]()))
    cfg.dominates shouldBe expectedDominates
  }
}
