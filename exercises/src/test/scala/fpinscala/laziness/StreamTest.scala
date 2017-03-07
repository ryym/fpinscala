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
}
