package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] {
  override def toString: String = "[ ]"
}

case class Cons[+A](head: A, tail: List[A]) extends List[A] {
  override def toString: String = {
    def loop(l: List[A]): String = l match {
      case Nil         => ""
      case Cons(x, xs) => s", $x${loop(xs)}"
    }

    s"[$head${loop(tail)}]"
  }

}

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match { // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) =>
        x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, t) => t
    case Nil        => Nil
  }

  //  def setHead[A](l: List[A], h: A): List[A] = Cons(h, l)
  //
  //  def drop[A](l: List[A], n: Int): List[A] = ???
  //
  //  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = ???
  //
  //  def init[A](l: List[A]): List[A] = ???

  def length[A](l: List[A]): Int = foldLeft(l, 0)((len, _) => len + 1)

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil         => z
      case Cons(y, ys) => foldLeft(ys, f(z, y))(f)
    }

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((r, x) => Cons(x, r))

  def foldRightViaLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((l, x) => f(x, l))

  def flatten[A](list: List[List[A]]): List[A] =
    foldLeft(list, List[A]())(append)

  def plusOne(list: List[Int]): List[Int] = list match {
    case Cons(h, t) => Cons(h + 1, plusOne(t))
    case Nil        => Nil
  }

  def map[A, B](l: List[A])(f: A => B): List[B] = l match {
    case Cons(h, t) => Cons(f(h), map(t)(f))
    case Nil        => Nil
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Cons(h, t) if f(h) => Cons(h, filter(t)(f))
    case Cons(_, t)         => filter(t)(f)
    case Nil                => Nil
  }

  //  todo: not efficient
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))

  def startsWith[A](list: List[A], prefix: List[A]): Boolean = list match {
    case Cons(xp, xsp) =>
      prefix match {
        case Cons(`xp`, ysb) => startsWith(xsp, ysb)
        case _ @Cons(_, _)   => false
        case Nil             => true
      }
    case _ => prefix == list
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case _ if startsWith(sup, sub) => true
    case Cons(_, t)                => hasSubsequence(t, sub)
    case Nil                       => false
  }
}

object Main extends App {

  import List._

  println(length(List(1, 2, 3, 4, 4, 5, "asv", "bfg")))

  println(reverse(List(1, 2, 3, 4, 4, 5, "asv", "bfg")))

  println(flatten(List(List(1,2,3), List(4, 5,6,7))))

  println(map(List(1, 2, 3))(_ + 2))

  println(filter(List(1, 2, 3, 4))(_ > 2))

  println(flatMap(List(1, 2, 3))(i => List(i, i)))

  println(hasSubsequence(List(1, 2, 3, 4, 5), List(1, 3)))

  println(hasSubsequence(List(1, 2, 1, 2, 1, 2, 3), List(1, 2, 1, 2, 3)))

}
