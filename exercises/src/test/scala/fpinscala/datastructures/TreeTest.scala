import org.scalatest._
import fpinscala.datastructures.{Tree, Leaf, Branch}

class TreeTest extends FunSuite with Matchers {
  test("size computes size of a given tree") {
    val tree = Branch(
      Branch(Leaf("a"), Leaf("b")),
      Branch(Leaf("c"), Leaf("d"))
    )
    Tree.size(tree) should equal (7)
    Tree.size(Leaf(0)) should equal (1)
  }

  test("maximum finds a max integer") {
    val tree = Branch(
      Branch(Leaf(2), Branch(Leaf(6), Leaf(3))),
      Branch(
        Branch(Leaf(4), Leaf(0)),
        Branch(Leaf(1), Branch(Leaf(9), Leaf(7)))
      )
    )
    Tree.maximum(tree) should equal (9)
    Tree.maximum(Leaf(-1)) should equal (-1)
  }

  test("depth measures longest path length") {
    val tree = Branch(
      Leaf(1),
      Branch(
        Leaf(1),
        Branch(Leaf(1), Branch(Leaf(1), Leaf(1)))
      )
    )
    Tree.depth(tree) should equal (5)
  }

  test("map maps each values") {
    val tree = Branch(Leaf(10), Branch(Leaf(5), Leaf(8)))
    val expected = Branch(Leaf("20"), Branch(Leaf("10"), Leaf("16")))
    Tree.map(tree)(n => (n * 2).toString) should equal (expected)
  }
}
