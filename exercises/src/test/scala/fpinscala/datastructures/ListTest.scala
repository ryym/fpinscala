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

  test("init selects items except a last one") {
    List.init(List(1, 2, 3, 4)) should equal (List(1, 2, 3))
  }

  test("foldRight(List, Nil)(Cons(_, _))") {
    List.foldList(List(1, 2, 3, 4)) should equal (List(1, 2, 3, 4))
  }

  test("length computes a length of given list") {
    List.length(List(0, 0, 0, 0, 0)) should equal (5)
    List.length(Nil) should equal (0)
  }

  test("foldLeft folds from left to right") {
    val cs = List('a', 'b', 'c')
    List.foldLeft(cs, "")(_ + _) should equal ("abc")
  }

  test("someL somes up numbers by foldLeft") {
    List.sumL(List(3, 2, 4, 7)) should equal (16)
  }

  test("productL products numbers by foldLeft") {
    List.productL(List(1, 2, 3, 4, 5)) should equal (120)
  }

  test("lengthL computes a length of given list") {
    List.lengthL(List(0, 0, 0, 0, 0)) should equal (5)
  }

  test("reverse reverses given list") {
    List.reverse(List(0, 1, 2, 3)) should equal (List(3, 2, 1, 0))
    List.reverse(Nil) should equal (Nil)
  }
}
