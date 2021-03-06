import org.scalatest._
import fpinscala.state._

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

  test("sequence maps list of state to state with result list") {
    val states = List(1, 2, 3, 4).map(n => {
      State[List[Int], Int](ns => (n, (n * 10) :: ns))
    })

    State.sequence(states).run(List(0)) should equal (
      List(1, 2, 3, 4),
      List(40, 30, 20, 10, 0)
    )
  }

  test("simulateMachine") {
    var inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
    val result = Machine.simulateMachine(inputs).run(Machine.init(5, 10))
    result should equal ((14, 1), Machine(true, 1, 14))
  }
}
