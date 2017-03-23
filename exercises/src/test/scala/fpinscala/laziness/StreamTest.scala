import org.scalatest._
import fpinscala.laziness.{Stream, Empty, Cons}

class StreamTest extends FunSuite with Matchers {
  import Stream.{cons => c, empty => emp}

  type ConstantMethod[A] = A => Stream[A]
  def testConstant[A](a: A)(constant: ConstantMethod[A]): Unit = {
    val st = constant(a)
    st.take(4).toList should equal (List(a, a, a, a))
    st.headOption should equal (Some(a))
  }

  def testFrom(from: Int => Stream[Int]): Unit = {
    from(3).take(3).toList should equal (List(3, 4, 5))
    from(-20).take(4).toList should equal (List(-20, -19, -18, -17))
  }

  def testFibs(fibs: Stream[Int]): Unit = {
    fibs.take(7).toList should equal (List(0, 1, 1, 2, 3, 5, 8))
    fibs.drop(2).take(2).toList should equal (List(1, 2))
  }

  type MapMethod[A, B] = Stream[A] => (A => B) => Stream[B]
  def testMap(map: MapMethod[Int, String]): Unit = {
    val st = c(0, c(1, c(2, emp)))
    val st2 = map(st)(_.toString)
    st2.toList should equal (List("0", "1", "2"))
  }

  type TakeMethod[A] = (Stream[A], Int) => Stream[A]
  def testTake(take: TakeMethod[Int]): Unit = {
    var i = 0
    def incl: Int = { i += 1; i }
    val st = c(incl, c(incl, c(incl, emp)))

    take(st, 2).toList should equal (List(1, 2))
    i should equal (2)
  }

  type TakeWhileMethod[A] = Stream[A] => (A => Boolean) => Stream[A]
  def testTakeWhile(takeWhile: TakeWhileMethod[Int]): Unit = {
    var i = 0
    def incl(n: Int)(): Int = { i += 1; n }
    val st = c(incl(0), c(incl(2), c(incl(3), c(incl(4), emp))))

    takeWhile(st)(_ % 2 == 0).toList should equal (List(0, 2))
    i should equal (3)
    takeWhile(st)(_ >= 0).toList should equal (List(0, 2, 3, 4))
  }

  test("toList evaluates all elements and map to list") {
    var i = 0
    def incl: Int = { i += 1; 1 }
    val st = c(incl, c(incl, c(incl, emp)))

    i should equal (0)
    st.toList should equal (List(1, 1, 1))
    i should equal (3)
  }

  test("take takes first nth elements") {
    testTake((s, i) => s.take(i))
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
    testTakeWhile(s => p => s.takeWhile(p))
  }

  test("map maps to another stream") {
    testMap(s => f => s.map(f))
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
    testConstant(8)(Stream.constant)
  }

  test("from creates incremental number list") {
    testFrom(Stream.from)
  }

  test("fibs creates infinite fibonacchi list") {
    testFibs(Stream.fibs)
  }

  test("unfold creates stream corecursively") {
    val st = Stream.unfold(0)(n => if (n == 5) None else Some(n, n + 1))
    st.toList should equal (List(0, 1, 2, 3, 4))
  }

  test("onesViaUnfold repeats 1 infinitely") {
    Stream.onesViaUnfold.take(3).toList should equal (List(1, 1, 1))
  }

  test("constantViaUnfold creates infinite stream by given value") {
    testConstant(8)(Stream.constantViaUnfold)
  }

  test("fromViaUnfold creates incremental number list") {
    testFrom(Stream.fromViaUnfold)
  }

  test("fibsViaUnfold creates infinite fibonacchi list") {
    testFibs(Stream.fibsViaUnfold)
  }

  test("mapViaUnfold maps to another stream") {
    testMap(s => f => s.mapViaUnfold(f))
  }

  test("takeViaUnfold takes first nth elements") {
    testTake((s, i) => s.takeViaUnfold(i))
  }

  test("takeWhileViaUnfold takes elements while predicate returns true") {
    testTakeWhile(s => p => s.takeWhileViaUnfold(p))
  }
  
  test("zipWith zips two streams") {
    val st1 = c(0, c(1, c(2, emp)))
    val st2 = c(2, c(1, c(0, emp)))

    st1.zipWith(st2)((a, b) => (a, b)).toList should equal (
      List((0, 2), (1, 1), (2, 0))
    )

    val longst = c(4, c(3, st2))
    st1.zipWith(longst)((a, b) => (a, b)).toList should equal (
      List((0, 4), (1, 3), (2, 2))
    )
  }

  test("zipAll zips all elements") {
    val st1 = c(0, c(1, c(2, emp)))
    val st2 = c(2, c(1, c(0, emp)))
    st1.zipAll(st2).toList should equal (
      List((Some(0), Some(2)), (Some(1), Some(1)), (Some(2), Some(0)))
    )

    val longst = c(4, c(3, st2))
    st1.zipAll(longst).toList should equal (
      List(
        (Some(0), Some(4)),
        (Some(1), Some(3)),
        (Some(2), Some(2)),
        (None, Some(1)),
        (None, Some(0))
      )
    )
    longst.zipAll(st1).toList should equal (
      List(
        (Some(4), Some(0)),
        (Some(3), Some(1)),
        (Some(2), Some(2)),
        (Some(1), None),
        (Some(0), None)
      )
    )
  }

  test("startsWith determines stream starts with another one") {
    val st1 = c(0, c(1, c(2, emp)))
    val st2 = c(0, c(1, emp))
    st1.startsWith(st2) should equal (true)
    st1.startsWith(Empty) should equal (true)
    st2.startsWith(st1) should equal (false)
  }
}
