import org.scalatest._
import java.util.concurrent.Executors
import fpinscala.parallelism.Par

class ParTest extends FunSuite with Matchers {
  import Par._

  val es = Executors.newFixedThreadPool(10)

  test("parBisect: get max value") {
    val vec = Vector(3, 5, 8, 1, 4, 2)
    val task = parBisect(vec)(-1)((a, b) => if (a > b) a else b)
    Par.run(es)(task).get should equal (8)
  }
}

