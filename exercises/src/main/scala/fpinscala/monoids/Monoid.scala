package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.state.RNG.Simple
import fpinscala.testing.Prop._
import fpinscala.testing.Gen

import language.higherKinds
import scala.util.Random

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero                       = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero                         = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    val zero: Int                          = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    val zero: Int                 = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    val zero: Boolean                         = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    val zero: Boolean                         = true
  }

  def optionMonoid[A: Monoid]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] =
      (a1, a2) match {
        case (Some(value1), Some(value2)) => Some(implicitly[Monoid[A]].op(value1, value2))
        case (someValue1, None)           => someValue1
        case (_, someValue2)              => someValue2
      }
    def zero: Option[A] = Option.empty
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a1 andThen a2
    def zero: A => A                       = identity
  }

  def flip[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(a1: A, a2: A): A = m.op(a2, a1)
    def zero: A             = m.zero
  }

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    identityLaw(m, gen) && associativityLaw(m, gen)

  def identityLaw[A](m: Monoid[A], gen: Gen[A]): Prop = forAll(gen) { a =>
    m.op(m.zero, a) == a && m.op(a, m.zero) == a
  }

  def associativityLaw[A](m: Monoid[A], gen: Gen[A]): Prop = forAll(gen ** gen ** gen) {
    case ((a, b), c) => m.op(m.op(a, b), c) == m.op(a, m.op(b, c))
  }

//  def trimMonoid(s: String): Monoid[String] = ???
//
//  def concatenate[A](as: List[A], m: Monoid[A]): A =
//    ???

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.map(f).foldLeft(m.zero)(m.op)

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(a => f(a, _))(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, flip(endoMonoid[B]))(a => f(_, a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (as.size < 2) {
      foldMap(as.toList, m)(f)
    } else {
      val (lAs, rAs) = as.splitAt(as.size / 2)
      m.op(foldMapV(lAs, m)(f), foldMapV(rAs, m)(f))
    }

  def ordered(ints: IndexedSeq[Int]): Boolean =
    foldMap(
      ints.toList,
      new Monoid[(Int, Boolean)] {
        def op(a1: (Int, Boolean), a2: (Int, Boolean)): (Int, Boolean) =
          (a1, a2) match {
            case ((i1, isOrdered), (i2, _)) if isOrdered => (i2, i2 >= i1)
            case _                                       => (Int.MinValue, false)
          }
        def zero: (Int, Boolean) = (Int.MinValue, true)
      }
    )(i => (i, true))._2

  sealed trait WC
  case class Stub(chars: String)                            extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  import fpinscala.parallelism.Nonblocking.Par._
  import fpinscala.parallelism.Nonblocking._

  def par[A](m: Monoid[A]): Monoid[Par[A]] =
    new Monoid[Par[A]] {
      def op(a1: Par[A], a2: Par[A]): Par[A] = a1.map2(a2)(m.op)
      def zero: Par[A]                       = Par.unit(m.zero)
    }

  // fixme: no parMap for Nonblocking Par
  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    foldMapV(v, par(m))(a => Par.delay(f(a)))

  //    Par
//      .flatMap(Par.sequenceBalanced(v.map(a => Par.delay(f(a))))) { s =>
//        .sequenceBalanced(

//          v.map(a => )(a => Par.delay(f(a))))
//        )

//      }

//    if (v.size < 2) {
//      Par.delay(foldMap(v.toList, m)(f))
//    } else {
//      val (lAs, rAs) = v.splitAt(v.size / 2)
//      par(m).op(parFoldMap(lAs, m)(f), parFoldMap(rAs, m)(f))
//    }

  val wcMonoid: Monoid[WC] = new Monoid[WC] {

    def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(w1), Stub(w2))                   => Stub(w1 concat w2)
      case (Stub(w1), Part(ls2, ws, rd2))         => Part(w1 concat ls2, ws, rd2)
      case (Part(ls1, ws, rd1), Stub(w2))         => Part(ls1, ws, rd1 concat w2)
      case (Part(ls1, ws1, _), Part(_, ws2, rs2)) => Part(ls1, ws1 + ws2 + 1, rs2)
    }

    def zero: WC = Stub("")

  }

  def count(s: String): Int = ???

  // ==================================

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    ???

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] =
    ???

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    ???

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    ???
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    ???

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    ???

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    ???

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    ???

  def toList[A](as: F[A]): List[A] =
    ???
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    ???
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    ???
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    ???
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    ???
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    ???
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    ???
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    ???
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    ???
}

sealed trait Tree[+A]
case class Leaf[A](value: A)                        extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    ???
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    ???
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    ???
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    ???
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    ???
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    ???
}

object TestMonoidLaw extends App {

  import Monoid._

  val rng = Simple(Random.nextLong())

  val asciiStrGen: Gen[String] = for {
    length      <- Gen.choose(1, 20)
    listOfChars <- Gen.listOfN(length, Gen.asciiChar)
  } yield listOfChars.mkString("")

//  run(monoidLaws(stringMonoid, strGen))
//  run(monoidLaws(intAddition, Gen.choose(Int.MinValue, Int.MaxValue)))

  val stubGen: Gen[Stub] = for {
    str <- asciiStrGen
  } yield Stub(str.replace(" ", ""))

  val partGen: Gen[Part] = for {
    lstub <- asciiStrGen
    count <- Gen.choose(0, 99999)
    rstub <- asciiStrGen
  } yield Part(lstub, count, rstub)

  val wcGen: Gen[WC] = Gen.oneOf(stubGen, partGen)

  run(monoidLaws(wcMonoid, wcGen))

}

object TestOrdered extends App {

  import Monoid._

  val l1 = IndexedSeq(-10, 2, 3, 4, 5, 1000)
  val l2 = IndexedSeq(-10000, 5, 10, 201, -9999, 100000)

  println(ordered(l1))
  println(ordered(l2))

}
