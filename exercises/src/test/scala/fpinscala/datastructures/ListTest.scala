import org.scalatest._
import fpinscala.datastructures.{List, Nil, Cons}

class ListTest extends FunSuite with Matchers {
  test("patern matching") {
    List.tryPatternMatching() should equal (3)
  }

  test("tail returns a new list except a head") {
    List.tail(List(1, 2, 3)) should equal (List(2, 3))
    List.tail(List(1)) should equal (Nil)
    List.tail(Nil) should equal (Nil)
  }

  test("setHead replaces a head") {
    List.setHead(List(1, 2, 3), -1) should equal (List(-1, 2, 3))
    List.setHead(List(1), 5) should equal (List(5))
    List.setHead(Nil, 10) should equal (Nil)
  }

  test("drop removes first n items from given list") {
    List.drop(List(1, 2, 3), 2) should equal (List(3))
    List.drop(Nil, 10) should equal (Nil)
  }

  test("dropWhile removes first n items while given predicate is true") {
    List.dropWhile[Int](List(1, 1, 1, 2), _ == 1) should equal (List(2))
    List.dropWhile[Int](List(2, 2), _ == 2) should equal (Nil)
    List.dropWhile[Int](List(2, 2), _ == 3) should equal (List(2, 2))
    List.dropWhile[Nothing](Nil, (_) => true) should equal (Nil)
  }
}
