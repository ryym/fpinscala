import org.scalatest._

import fpinscala.gettingstarted.PolymorphicFunctions.isSorted

class IsSortedTest extends FunSuite with Matchers {
  test("isSorted checks if an array is sorted or not") {
    isSorted[Int](Array(1, 2, 3, 4), _ > _) should equal (true)
    isSorted[Int](Array(1, 3, 2, 4), _ > _) should equal (false)
    isSorted[Char](Array('a', 'b', 'c'), _ > _) should equal (true)
  }
}
