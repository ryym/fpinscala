import org.scalatest._
import fpinscala.errorhandling.{Either, Left, Right}

class EitherTest extends FunSuite with Matchers {
  test("map maps right value") {
    Right(1) map (_ * 2) should equal (Right(2))
    (Left("err"):Either[String, Int]) map (_ * 2) should equal (Left("err"))
  }

  test("flatMap maps and flattens") {
    val double = (n: Int) => Right(n * 2)
    Right(1) flatMap (double) should equal (Right(2))
    Right(1) flatMap (_ => Left("err")) should equal (Left("err"))
    (Left("err"):Either[String, Int]) flatMap (double) should equal (Left("err"))
  }

  test("orElse returns given one if this is left") {
    Right(1) orElse Right(2) should equal (Right(1))
    Left("err") orElse Right(2) should equal (Right(2))
  }

  test("map2 maps two either values") {
    Right(1).map2(Right(2))(_ + _) should equal (Right(3))
    Right(1).map2(Left("err"))(_ + _) should equal (Left("err"))
  }
}
