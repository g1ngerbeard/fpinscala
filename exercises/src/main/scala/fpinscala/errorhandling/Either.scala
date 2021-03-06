package fpinscala.errorhandling


import scala.{Either => _, Left => _, Option => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(v) => Right(f(v))
    case l@Left(_) => l
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => f(v)
    case l@Left(_) => l
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case r@Right(_) => r
    case _ => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this.flatMap(v => b.map(vb => f(v, vb)))
}

case class Left[+E](get: E) extends Either[E, Nothing]

case class Right[+A](get: A) extends Either[Nothing, A]

object Either {

  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    def continue(rest: List[A], res: List[B]): Either[E, List[B]] = rest match {
      case x :: xs => f(x) match {
        case Right(value) => continue(xs, res :+ value)
        case l@ Left(_) => l
      }
      case Nil => Right(res)
    }

    continue(es, List.empty)
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(identity)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

}

object EitherTest extends App {

  import Either._

  println(Try("1".toInt).map(_ + 1))

  println(Try("a".toInt).map(_ + 1))

  println(Try("1".toInt).flatMap(i => Try("2".toInt)))

  println(sequence(List(Try("1".toInt), Try("2".toInt))))

  println(traverse(List("a","2"))(s => Try(s.toInt)))
}