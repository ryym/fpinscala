package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tryPatternMatching(): Int = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  // NOTE:
  // Scala can't infer argument types from previous arguments,
  // but it can do from previous argument groups. So we can omit type annotations
  // using a curried function.
  // e.g.) map(List(1,2,3))(a => a + 1)  // 'a' doesn't have a type annotation.

  // The flow of foldRight:
  //   foldRight(Cons(1, Cons(2, Cons(3, Nil))), 0)((x, y) => x + y)
  //   1 + foldRight(Cons(2, Cons(3, Nil)), 0)((x, y) => x + y)
  //   1 + (2 + foldRight(Cons(3, Nil), 0))((x, y) => x + y)
  //   1 + (2 + (3 + foldRight(Nil, 0)))((x, y) => x + y)
  //   1 + (2 + (3 + (0)))
  //   6
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // List is values folded by Cons:
  //   foldRight(List(1, 2, 3), Nil)(Cons(_, _))
  //   foldRight(Cons(1, Cons(2, Cons(3, Nil))), Nil)(Cons(_, _))
  //   Cons(1, foldRight(Cons(2, Cons(3, Nil)), Nil))(Cons(_, _))
  //   Cons(1, Cons(2, foldRight(Cons(3, Nil), Nil)))(Cons(_, _))
  //   Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil))))(Cons(_, _))
  //   Cons(1, Cons(2, Cons(3, Nil)))
  def foldList[A](l: List[A]): List[A] =
    foldRight(l, Nil:List[A])(Cons(_, _))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  def dropUsingTail[A](l: List[A], n: Int): List[A] =
    if (n == 0 || l == Nil) l
    else drop(tail(l), n - 1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, i) => i + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def go(l: List[A], z: B): B = l match {
      case Nil => z
      case Cons(h, t) => go(t, f(z, h))
    }
    return go(l, z)
  }

  def sumL(l: List[Int]): Int =
    foldLeft(l, 0)(_ + _)

  def productL(l: List[Double]): Double =
    foldLeft(l, 1.0)(_ * _)

  def lengthL[A](l: List[A]): Int =
    foldLeft(l, 0)((i, _) => i + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil:List[A])((l, x) => Cons(x, l))

  def foldRightL[A,B](l: List[A], z: B)(f: (A, B) => B): B = {
    // Wrap each operation as a function to delay each evaluation.
    foldLeft(l, (b:B) => b)((b2b, a) => b => b2b(f(a, b)))(z)
  }
  // def delay0(b) = b => b
  // def delay1(b) = delay0(Cons(1, b))
  // def delay2(b) = delay1(Cons(2, b))
  // def delay3(b) = delay2(Cons(3, b))
  // delay3(Nil)

  def append2[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l2, l1)(Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)

  def addOneEach(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int])((n, ns) => Cons(n + 1, ns))

  def doublesToStrings(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String])((d, ss) => Cons(d.toString, ss))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((a, bs) => Cons(f(a), bs))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil:List[A])((a, as) => if (f(a)) Cons(a, as) else as)
}
