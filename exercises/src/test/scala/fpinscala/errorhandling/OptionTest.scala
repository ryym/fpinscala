import org.scalatest._
import fpinscala.errorhandling.{Option, Some, None}

class OptionTest extends FunSuite with Matchers {
  test("map maps its value") {
    Some(3).map(_ * 2) should equal (Some(6))
    (None: Option[Int]).map(_ * 2) should equal (None)
  }

  test("getOrElse returns its value or given default value") {
    Some(1).getOrElse(0) should equal (1)
    None.getOrElse(0) should equal (0)
  }

  test("flatMap maps and flatten optional value") {
    val mayDouble = (n: Int) => Some(n * 2)
    Some(3).flatMap(mayDouble) should equal (Some(6))
    (None: Option[Int]).flatMap(mayDouble) should equal (None)
  }

  test("orElse returns itself or default optional value") {
    Some(1).orElse(Some(10)) should equal (Some(1))
    None.orElse(Some(10)) should equal (Some(10))
  }

  test("filter returns None if predicate is false") {
    Some(4).filter(_ % 2 == 0) should equal (Some(4))
    Some(3).filter(_ % 2 == 0) should equal (None)
  }
}
