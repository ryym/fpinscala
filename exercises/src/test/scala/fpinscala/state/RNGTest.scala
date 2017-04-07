import org.scalatest._
import fpinscala.state.{RNG}

class RNGTest extends FunSuite with Matchers {
  case class FakeRNG(int: Int) extends RNG {
    val nextInt = (int, this)
  }

  test("nonNegativeInt returns non negative int randomly") {
    RNG.nonNegativeInt(FakeRNG(10))._1 should equal (10)
    RNG.nonNegativeInt(FakeRNG(-55))._1 should equal (54)
    RNG.nonNegativeInt(FakeRNG(Int.MaxValue))._1 should equal (Int.MaxValue)
    RNG.nonNegativeInt(FakeRNG(Int.MinValue))._1 should equal (Int.MaxValue)
  }
}
