package fpinscala.laziness

import Stream._

import scala.util.Random

trait Stream[+A] {

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case Empty => List.empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case s => s
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (p(h)) cons(h, t) else empty
    )

  def takeWhile2(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = foldRight(Option.empty[A])((h, _) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h, t) => cons(f(h), t))

  def map2[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (p(h)) cons(h, t)
      else t
    )

  def append[B >: A](a: => B): Stream[B] = foldRight(cons(a, empty))(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])(
    (h, t) => f(h).foldRight(t)(cons(_, _))
  )

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
    case (Cons(h1, t1), _) => Some(((Some(h1()), None), (t1(), empty)))
    case (_, Cons(h2, t2)) => Some(((None, Some(h2())), (empty, t2())))
    case (_, _) => None
  }

  def startsWith[B](s: Stream[B]): Boolean = zipAll(s).foldRight(true) { (h, rec) =>
    h match {
      case (Some(hThis), Some(hThat)) => hThis == hThat && rec
      case (_, None) => true
      case _ => false
    }
  }

  def tails: Stream[Stream[A]] = unfold(this) {
    case cons@Cons(_, t) => Some((cons, t()))
    case _ => None
  }

  def hasSubsequence[B >: A](s: Stream[B]): Boolean =
    tails exists (_ startsWith s)
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = unfold(a)(s => Some(s, s))

  def from(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))

  def fibs(): Stream[Int] = unfold((0, 1)) {
    case (s1, s2) => Some((s1, (s2, s1 + s2)))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case _ => empty
  }

}

object StreamTest extends App {

  //  def rnd(): Int = Random.nextInt()
  //  val unevaluated = Cons(rnd, () => Cons(rnd, () => Cons(rnd, () => Empty)))
  //
  //  println(unevaluated.toList)
  //  println(unevaluated.take(2).toList)
  //  println(unevaluated.drop(2).toList)
  //
  val ints = Stream(1, 2, 3, 5, 50, 51, 1000, 2332, 13)

  val ints2 = Stream(1, 2, 3, 4)

  println(ints.takeWhile(_ < 100).toList)
  println(ints.takeWhile2(_ < 100).toList)

  //  println(ints.headOption)
  println(ints.map2(_ + 1).toList)
  //  println(ints.append(20001).toList)
  //  println(ints.flatMap(i => ints2.map(_ * i)).toList)

  println(ints.zipAll(ints2).toList)

  //  println(ones.take(10).toList)
  //
  //  println(from(2).take(10).toList)
  //
  //    println(fibs().take(10).toList)

  println(ints startsWith ints2)

  println(ints2.tails.map(_.toList).toList)

  require(ints hasSubsequence Stream(5, 50, 51))

  require(!(ints hasSubsequence Stream(5, 50, 1000)))
}