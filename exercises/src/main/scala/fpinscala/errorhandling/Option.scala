package fpinscala.errorhandling


import scala.annotation.tailrec
import scala.{Either => _, Option => _, Some => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(value) => Some(f(value))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(v) => v
    case _ => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = flatMap(v => if (f(v)) Some(v) else None)

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch {
      case e: Exception => 43
    } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch {
      case e: Exception => 43
    }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(va => b.map(vb => f(va, vb)))

  //  def sequence[A](a: List[Option[A]]): Option[List[A]] = ???

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {

    @tailrec
    def loop(l: List[A], ml: List[B]): Option[List[B]] = l match {
      case x :: xs => f(x) match {
        case Some(v) => loop(xs, ml :+ v)
        case None => None
      }
      case Nil => Some(ml)
    }

    loop(a, List.empty)
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case e: Exception => None
    }

  def parseInts(a: List[String]): Option[List[Int]] = traverse(a)(i => Try(i.toInt))
}


object OptionTest extends App {

  import Option._

  val opt = Some(123)

  println(opt)
  println(opt.filter(_ < 12))
  println(opt.map(_ + 1))
  println(opt.flatMap(Some(_)))

  println(map2(Some(1), Some(2))(_ + _))

  println(parseInts(List("1", "2", "3")))
  println(parseInts(List("1", "2", "a")))
}