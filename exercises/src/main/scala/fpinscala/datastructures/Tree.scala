package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size(tree: Tree[Any]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(n) => n
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth(tree: Tree[Any]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](tree: Tree[A])(l: A => B)(b: (B, B) => B): B = tree match {
    case Leaf(x) => l(x)
    case Branch(left, right) => b(fold(left)(l)(b), fold(right)(l)(b))
  }

  def sizeF(tree: Tree[Any]) =
    fold(tree)(_ => 1)(_ + _ + 1)

  def maximumF(tree: Tree[Int]) =
    fold(tree)(n => n)(_ max _)

  def depthF(tree: Tree[Any]) =
    fold(tree)(_ => 1)((a, b) => 1 + (a max b))

  def mapF[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(x => Leaf(f(x)): Tree[B])((l, r) => Branch(l, r))
}
