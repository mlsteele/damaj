package compile
import org.scalatest._

class GraphColorSpec extends FlatSpec with Matchers {
  case class Node(name: String)
  case class Color(name: String)

  val allColors = List(Color("red"), Color("green"), Color("blue"))

  def mkNodes(howMany: Int): Seq[Node] =
    (0 to (howMany-1)).map{ i => Node(i.toString) }

  def colorAndValidate[X, C](nodes: Set[X], neighbors: X => Set[X], colors: Seq[C]): Map[X, C] = {
    val coloring = GraphColor.color(nodes, neighbors, colors)
    GraphColor.validate(nodes, neighbors, colors, coloring) shouldBe Set.empty
    return coloring
  }

  // "GraphColor" should "color one node" in {
  it should "color one node" in {
    val nodes = mkNodes(1)
    def neighbors(x: Node): Set[Node] = Set()
    val colors = allColors.take(2)

    val expected: Map[Node, Color] = Map(
      nodes(0) -> colors(0)
    )

    colorAndValidate[Node, Color](nodes.toSet, neighbors, colors) shouldBe expected
  }

  it should "color two nodes" in {
    val nodes = mkNodes(2)
    def neighbors(x: Node): Set[Node] = Set()
    val colors = allColors.take(2)

    val result = colorAndValidate[Node, Color](nodes.toSet, neighbors, colors)
    result should contain key nodes(0)
  }

  it should "spill extra regs" in {
    val nodes = mkNodes(3)
    def neighbors(x: Node): Set[Node] = (nodes.toSet -- Set(x))
    val colors = allColors.take(2)

    val result = colorAndValidate[Node, Color](nodes.toSet, neighbors, colors)
    result.size shouldBe 2
  }

  it should "work on this one from the slides" in {
    // This graph is from the slides F14-lecture-12.pdf p.62
    val nodes: Seq[Node] = mkNodes(5)
    def neighbors(x: Node): Set[Node] = nodes.indexOf(x) match {
      case 0 => Set(1, 2, 3).map{nodes(_)}
      case 1 => Set(0, 2, 3).map{nodes(_)}
      case 2 => Set(0, 1, 3, 4).map{nodes(_)}
      case 3 => Set(0, 1, 2, 4).map{nodes(_)}
      case 4 => Set(2, 4).map{nodes(_)}
    }
    val colors = allColors.take(3)

    val result = colorAndValidate[Node, Color](nodes.toSet, neighbors, colors)
    // Only 4 of the 5 nodes can be colored with 3 colors.
    result.size shouldBe 4
    result.get(nodes(4)) shouldBe ('defined)
  }
}
