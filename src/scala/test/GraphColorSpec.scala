package compile
import org.scalatest._

class GraphColorSpec extends FlatSpec with Matchers {
  class Node {
    val id = SlugGenerator.id(this)
    override def toString = s"Node($id)"
  }
  case class Color(name: String)

  val allColors = List(Color("red"), Color("green"), Color("blue"))

  "GraphColor" should "color one node" in {
    val nodes = List(new Node)
    def neighbors(x: Node): Set[Node] = Set()
    val colors = allColors.take(2)

    val expected: Map[Node, Color] = Map(
      nodes(0) -> colors(0)
    )

    GraphColor.color[Node, Color](nodes.toSet, neighbors, colors) shouldBe expected
  }

  it should "color two nodes" in {
    val nodes = List(new Node, new Node)
    def neighbors(x: Node): Set[Node] = Set()
    val colors = allColors.take(2)

    val result = GraphColor.color[Node, Color](nodes.toSet, neighbors, colors)
    result should contain key nodes(0)
  }

  it should "spill extra regs" in {
    val nodes = List(new Node, new Node, new Node)
    def neighbors(x: Node): Set[Node] = (nodes.toSet -- Set(x))
    val colors = allColors.take(2)

    val result = GraphColor.color[Node, Color](nodes.toSet, neighbors, colors)
    result.size shouldBe 2
  }
}
