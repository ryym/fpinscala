import org.scalatest._
import fpinscala.state.{State}

class StateTest extends FunSuite with Matchers {
  test("map maps value without changing state") {
    val state = State[String, Int](s => (2, s))
    state.map(_ * 3).run("a") should equal (6, "a")
  }

  test("map2 maps using another state") {
    val s1 = State[String, Int](s => (2, s))
    val s2 = State[String, Int](s => (4, s))
    s1.map2(s2)(_ * _).run("a") should equal (8, "a")
  }

  test("flatMap maps function which returns State") {
    val state = State[String, List[Int]](s => (List(1, 2, 3), s))
    def len(xs: List[_]): State[String, Int] = State(s => (xs.length, s))
    state.flatMap(len).run("") should equal (3, "")
  }
}
