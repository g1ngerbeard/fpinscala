package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(_ + _)

  def maximum(tree: Tree[Int]): Int = fold(tree)(identity)(_ max _)

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold[A, Tree[B]](tree)(v => Leaf(f(v)))(Branch(_, _))

  def fold[A, B](tree: Tree[A])(m: A => B)(r: (B, B) => B): B = tree match {
    case Branch(left, right) => r(fold(left)(m)(r), fold(right)(m)(r))
    case Leaf(value)         => m(value)
  }

}

object TreeTest extends App {

  import Tree._

  val tree = Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(112))), Leaf(0))

  assert(size(tree) == 4)
  assert(maximum(tree) == 112)

  println(map(tree)(_ + 1))

}
