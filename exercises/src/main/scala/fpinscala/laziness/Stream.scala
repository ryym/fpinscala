package fpinscala.laziness

import Stream._

// Using Stream, we can combine methods like `map` and `filter`
// without creating intermediate object (such as `list.map(_ + 1).filter(_ < 0)`)
// because the elements are evaluated only when it is really needed (non-strictness).

trait Stream[+A] {

  // Not stack safe.
  // def toList(): List[A] = this match {
  //   case Empty => Nil
  //   case Cons(h, t) => h() :: t().toList()
  // }

  def toList: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }
    go(this)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.
    // Note that this is not stack safe if the predicate match no elements.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
    case _ => Empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, as) => if (f(a)) cons(a, as) else as)

  // We can define `find` using `filter`. This method stops iteration
  // when the first matching element is found.
  def findViaFilter(p: A => Boolean): Option[A] =
    filter(p).headOption

  def append[AA >: A](that: Stream[AA]): Stream[AA] =
    foldRight(that)((a, s) => cons(a, s))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a) append b)

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this)(s => s match {
      case Cons(h, t) => Some((f(h())), t())
      case Empty => None
    })

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (s, i) => s match {
        case Cons(h, t) if i > 0 => Some((h(), (t(), i - 1)))
        case _ => None
      }
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this)(s => s match {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    })

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  // Return `Stream[A]` instead of `Empty`.
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(prev: Int, cur: Int): Stream[Int] = {
      cons(prev, go(cur, prev + cur))
    }
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty
  }

  def onesViaUnfold: Stream[Int] =
    unfold(null)(s => Some(1, s))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(null)(s => Some(a, s))

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(i => Some(i, i + 1))

  def fibsViaUnfold: Stream[Int] =
    unfold((0, 1)) { case (prev, cur) => Some(prev, (cur, prev + cur)) }
}
