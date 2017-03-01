import org.scalatest._
import fpinscala.datastructures.{Tree, Leaf, Branch}

class TreeTest extends FunSuite with Matchers {
  def testSize(size: Tree[Any] => Int): Unit = {
    val tree = Branch(
      Branch(Leaf("a"), Leaf("b")),
      Branch(Leaf("c"), Leaf("d"))
    )
    size(tree) should equal (7)
    size(Leaf(0)) should equal (1)
  }

  def testMaximum(maximum: Tree[Int] => Int): Unit = {
    val tree = Branch(
      Branch(Leaf(2), Branch(Leaf(6), Leaf(3))),
      Branch(
        Branch(Leaf(4), Leaf(0)),
        Branch(Leaf(1), Branch(Leaf(9), Leaf(7)))
      )
    )
    maximum(tree) should equal (9)
    maximum(Leaf(-1)) should equal (-1)
  }

  def testDepth(depth: Tree[Any] => Int): Unit = {
    val tree = Branch(
      Leaf(1),
      Branch(
        Leaf(1),
        Branch(Leaf(1), Branch(Leaf(1), Leaf(1)))
      )
    )
    depth(tree) should equal (5)
  }

  // XXX: In Scala, we can't define an anonymous function with generics..
  // testMap(map: [A, B](t: Tree[A]) => (A => B) => Tree[B])
  type TestTreeMapper = Tree[Int] => (Int => String) => Tree[String]
  def testMap(map: TestTreeMapper): Unit = {
    val tree = Branch(Leaf(10), Branch(Leaf(5), Leaf(8)))
    val expected = Branch(Leaf("20"), Branch(Leaf("10"), Leaf("16")))
    map(tree)(n => (n * 2).toString) should equal (expected)
  }

  test("size computes size of a given tree") {
    testSize(Tree.size)
  }

  test("maximum finds a max integer") {
    testMaximum(Tree.maximum)
  }

  test("depth measures longest path length") {
    testDepth(Tree.depth)
  }

  test("map maps each values") {
    testMap(Tree.map: TestTreeMapper)
  }

  test("sizeF computes size of a given tree using fold") {
    testSize(Tree.sizeF)
  }

  test("maximumF finds a max integer") {
    testMaximum(Tree.maximumF)
  }

  test("depthF measures longest path length") {
    testDepth(Tree.depthF)
  }

  test("mapF maps each values") {
    testMap(Tree.mapF: TestTreeMapper)
  }
}
