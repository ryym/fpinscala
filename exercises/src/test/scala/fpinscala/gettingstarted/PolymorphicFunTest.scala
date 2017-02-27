import org.scalatest._
import fpinscala.gettingstarted.PolymorphicFunctions._

class PolymorphicFunTest extends FunSuite with Matchers {
  test("isSorted checks if an array is sorted or not") {
    isSorted[Int](Array(1, 2, 3, 4), _ > _) should equal (true)
    isSorted[Int](Array(1, 3, 2, 4), _ > _) should equal (false)
    isSorted[Char](Array('a', 'b', 'c'), _ > _) should equal (true)
  }

  test("curry curries a function") {
    def some = (a: Int, b: Int) => a + b
    curry(some)(2)(4) should equal (6)
  }

  test("uncurry uncurries a function") {
    def some = (a: Int) => (b: Int) => a + b
    uncurry(some)(2, 4) should equal (6)
  }

  test("compose composes two functions") {
    def double = (a: Int) => a * 2
    val quadruple = compose(double, double)
    quadruple(3) should equal (12)
  }
}
