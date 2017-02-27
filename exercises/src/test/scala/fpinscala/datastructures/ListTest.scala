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

  test("foldRightL is a foldRight implemented by foldLeft") {
    val chars = List.foldRightL(List('a', 'b', 'c'), "")((c, cs) => cs + c)
    chars should equal ("cba")
  }

  test("append2 appends a list to another one") {
    val appended = List.append(List(1, 2, 3), List(4, 5, 6))
    appended should equal (List(1, 2, 3, 4, 5, 6))
  }

  test("concat concatinates a list of lists") {
    val lists = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
    List.concat(lists) should equal (List(1, 2, 3, 4, 5, 6, 7, 8, 9))
  }

  test("addOneEach adds one to each element of a digit list") {
    List.addOneEach(List(0, 1, 2, 3)) should equal (List(1, 2, 3, 4))
  }

  test("doublesToStrings converts each doulbe to string") {
    val doubles = List(0.0, 1.4, 5.2)
    List.doublesToStrings(doubles) should equal (List("0.0", "1.4", "5.2"))
  }

  test("map maps each element of a list") {
    List.map(List(0, 1, 2))(n => n + 1) should equal (List(1, 2, 3))
    List.map(List(0.0, 1.4))(d => d.toString) should equal (List("0.0", "1.4"))
  }

  test("filter filters a list by a given predicate") {
    List.filter(List(0, 1, 2, 3, 4))(_ % 2 == 0) should equal (List(0, 2, 4))
  }

  test("flatMap maps and flattens a list") {
    List.flatMap(List(1, 2, 3))(i => List(i, i)) should equal (List(1, 1, 2, 2, 3, 3))
  }

  test("filterViaFlatMap is a filter implemented based on flatMap") {
    def isEven(n: Int) = n % 2 == 0
    val nums = List(0, 1, 2, 3, 4)
    List.filterViaFlatMap(nums)(isEven) should equal (List.filter(nums)(isEven))
  }

  test("addPairwise adds each digit of two lists") {
    List.addPairwise(List(1, 2, 3), List(1, 2, 3)) should equal (List(2, 4, 6))
    List.addPairwise(List(1), List(1, 2, 3)) should equal (List(2))
  }
}
