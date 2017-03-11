import org.scalatest._
import fpinscala.laziness.{Stream, Empty, Cons}

class StreamTest extends FunSuite with Matchers {
  import Stream.{cons => c, empty => emp}

  test("toList evaluates all elements and map to list") {
    var i = 0
    def incl: Int = { i += 1; 1 }
    val st = c(incl, c(incl, c(incl, emp)))

    i should equal (0)
    st.toList should equal (List(1, 1, 1))
    i should equal (3)
  }

  test("take takes first nth elements") {
    var i = 0
    def incl: Int = { i += 1; i }
    val st = c(incl, c(incl, c(incl, emp)))

    st.take(2).toList should equal (List(1, 2))
    i should equal (2)
  }

  test("drop skips first nth elements") {
    var i = 0
    def incl(n: Int)(): Int = { i += 1; n }
    val st = c(incl(0), c(incl(1), c(incl(2), emp)))

    st.drop(2).toList should equal (List(2))
    i should equal (1)
  }

  test("forAll checks if predicate matches all elements lazily") {
    var i = 0
    def incl(n: Int)(): Int = { i += 1; n }
    val st = c(incl(0), c(incl(2), c(incl(4), emp)))

    st.forAll(_ == 0) should equal (false)
    i should equal (2)
    st.forAll(_ >= 0) should equal (true)
  }

  test("takeWhile takes elements while predicate returns true") {
    var i = 0
    def incl(n: Int)(): Int = { i += 1; n }
    val st = c(incl(0), c(incl(2), c(incl(3), c(incl(4), emp))))

    st.takeWhile(_ % 2 == 0).toList should equal (List(0, 2))
    i should equal (3)
    st.takeWhile(_ >= 0).toList should equal (List(0, 2, 3, 4))
  }
}
