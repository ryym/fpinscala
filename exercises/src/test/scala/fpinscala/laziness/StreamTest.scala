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

  test("map maps to another stream") {
    val st = c(0, c(1, c(2, emp)))
    val st2 = st.map(_.toString)
    st2.toList should equal (List("0", "1", "2"))
  }

  test("filter filters some elements") {
    val st = c(1, c(-1, c(2, emp)))
    val st2 = st.filter(_ < 0)
    st2.toList should equal (List(-1))
  }

  test("append appends two streams lazily") {
    var i = 0
    def incl(n: Int)(): Int = { i += 1; n }
    val st1 = c(incl(0), c(incl(2), emp))
    val st2 = c(incl(1), c(incl(3), emp))
    val st3 = st1 append st2

    // Only the first element is evaluated since Stream is non-strict.
    i should equal (1)
    st3.toList should equal (List(0, 2, 1, 3))
  }

  test("flatMap maps streams to another stream") {
    val st = c(0, c(1, c(2, emp)))
    val double = (n: Int) => c(n, c(n, emp))

    st.flatMap(double).toList should equal (List(0, 0, 1, 1, 2, 2))
  }

  test("constant creates infinite stream by given value") {
    val st = Stream.constant(true)
    st.take(4).toList should equal (List(true, true, true, true))
    st.headOption should equal (Some(true))
  }

  test("from creates incremental number list") {
    Stream.from(3).take(3).toList should equal (List(3, 4, 5))
    Stream.from(-20).take(4).toList should equal (List(-20, -19, -18, -17))
  }

  test("fibs creates infinite fibonacchi list") {
    Stream.fibs().take(7).toList should equal (List(0, 1, 1, 2, 3, 5, 8))
    Stream.fibs().drop(2).take(2).toList should equal (List(1, 2))
  }
}
