import org.scalatest._
import fpinscala.state.{RNG}

class RNGTest extends FunSuite with Matchers {
  import RNG._

  case class FakeRNG(int: Int) extends RNG {
    val nextInt = (int, this)
  }

  test("nonNegativeInt returns non negative int randomly") {
    nonNegativeInt(FakeRNG(10))._1 should equal (10)
    nonNegativeInt(FakeRNG(-55))._1 should equal (54)
    nonNegativeInt(FakeRNG(Int.MaxValue))._1 should equal (Int.MaxValue)
    nonNegativeInt(FakeRNG(Int.MinValue))._1 should equal (Int.MaxValue)
  }

  test("ints generates random int list") {
    ints(3)(FakeRNG(5))._1 should equal (List(5, 5, 5))
  }

  test("map2 maps two Rand") {
    map2[Int, Int, Int](
      _ => (5, FakeRNG(0)),
      _ => (8, FakeRNG(0))
    )(_ * _)(FakeRNG(0))._1 should equal (40)
  }

  test("sequence converts list of Rand to result list") {
    val result = sequence(List(unit(1), unit(2), unit(3)))(RNG.Simple(1))._1
    result should equal (List(1, 2, 3))
  }

  test("ints2 generates random int list") {
    ints2(3)(FakeRNG(5))._1 should equal (List(5, 5, 5))
  }
}
