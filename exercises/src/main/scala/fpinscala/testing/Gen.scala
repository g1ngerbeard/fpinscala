package fpinscala.testing

import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.laziness.Stream
import fpinscala.parallelism.Par.{Par, equal}
import fpinscala.state.RNG.{Simple, boundedInt}
import fpinscala.state._
import fpinscala.testing.Prop.Prop
//import fpinscala.testing.PropTest.maxProp

import scala.util.Random

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
 */

//trait Prop {
//
//  def check: Boolean
//
//  def &&(that: Prop): Prop = new Prop {
//    def check: Boolean = this.check && that.check
//  }
//
//}

object Prop {

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  case object Proved extends Result {
    def isFalsified: Boolean = false
  }

  type FailedCase   = String
  type SuccessCount = Int

  type TestCases = Int

  case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
    def &&(that: Prop): Prop =
      Prop((s, n, rng) => {
        val r1 = run(s, n, rng)

        if (r1.isFalsified) r1 else that.run(s, n, rng)
      })

    def ||(that: Prop): Prop =
      Prop((s, n, rng) => {
        val r1 = run(s, n, rng)

        if (r1.isFalsified) that.run(s, n, rng) else r1
      })
  }

  type MaxSize = Int

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (_, n, rng) =>
    randomStream(as)(rng)
      .zip(Stream.from(0))
      .take(n)
      .map {
        case (a, i) =>
          try {
            if (f(a)) Passed else Falsified(a.toString, i)
          } catch {
            case e: Exception => Falsified(buildMsg(a, e), i)
          }
      }
      .find(_.isFalsified)
      .getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop { (max, n, rng) =>
    val casesPerSize = (n + (max - 1)) / max

    val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))

    val prop: Prop = props
      .map(p =>
        Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
      })
      .toList
      .reduce(_ && _)

    prop.run(max, n, rng)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => println(s"! Falsified after $n passed tests:\n $msg")
      case Passed            => println(s"+ OK, passed $testCases tests.")
      case Proved            => println(s"+ OK, proved property.")
    }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Passed else Falsified("()", 0)
  }
}

case class Gen[+A](sample: State[RNG, A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.map(f).flatMap(_.sample))

  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => Gen.listOfN(n, this))

  def unsized: SGen[A] = SGen(_ => this)

  def **[B](g: Gen[B]): Gen[(A, B)] = (this map2 g)((_, _))

  def map2[B, C](gb: Gen[B])(f: (A, B) => C): Gen[C] = flatMap(a => gb.map(b => f(a, b)))

}

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(State(boundedInt(start, stopExclusive)))

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def asciiChar: Gen[Char] = Gen(State(RNG.asciiChar))

  def integer: Gen[Int] = Gen(State(RNG.int))

  def double: Gen[Double] = Gen(State(RNG.double))

  def pair[A](g: Gen[A]): Gen[(A, A)] = Gen(listOfN(2, g).sample.map { case Seq(a, b) => (a, b) })

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](ga: Gen[A], gb: Gen[A]): Gen[A] = boolean.flatMap(if (_) ga else gb)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val (gen1, w1) = g1
    val (gen2, w2) = g2
    val p          = w1 / (w1 + w2)

    double.flatMap(d => if (d < p) gen1 else gen2)
  }

  def oneOf[A](gen1: Gen[A], gen2: Gen[A]): Gen[A] =
    for {
      switch <- Gen.boolean
      res    <- if (switch) gen1 else gen2
    } yield res

}

case class SGen[+A](forSize: Int => Gen[A]) {

  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen(
    s => forSize(s).flatMap(a => f(a).forSize(s))
  )

  def map[B](f: A => B): SGen[B] = SGen(forSize(_).map(f))

}

object SGen {

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(Gen.listOfN(_, g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(s => Gen.listOfN(if (s == 0) 1 else s, g))

}

//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}

//trait SGen[+A] {
//
//}

object ** {
  def unapply[A, B](p: (A, B)) = Some(p)
}

object PropTest extends App {

  val rng = Simple(Random.nextLong())

  val g1 = Gen(State(RNG.double))
  val g2 = Gen.unit(0.07)

  val gu = Gen.weighted((g1, 0.1), (g2, 1.2))

  //  println(Gen.listOfN(50, gu).sample.run(rnd))

  val sgu = SGen.listOf(gu)

  println(sgu.forSize(10).sample.run(rng))

}

object ListSpec extends App {
  import Prop._
  import SGen._

  val smallInt = Gen.choose(0, 100)

  val maxProp: Prop = forAll(listOf1(smallInt)) { ns =>
    val sorted = ns.sorted
    ns.forall(sorted.head <= _)
  }

  run(maxProp)

}

object ParTest extends App {
  import Gen._
  import fpinscala.parallelism.Par

  val ES = Executors.newFixedThreadPool(10)

  val gen = Gen.choose(12, 12312)

  val p3 = Prop.check {
    equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )(ES).get
  }

  val S = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool)            -> .25
  )

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop = Prop.forAll(S ** g) { case s ** a => f(a)(s).get }

  def checkPar(p: => Par[Boolean]): Prop = forAllPar(Gen.unit({}))(_ => p)

  val p2 = checkPar(
    equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    ))

  Prop.run(p2)

  val parInt = Gen.choose(0, 10).map(Par.unit)
  val p4 = forAllPar(parInt) { pi =>
    equal(
      Par.map(pi)(identity),
      pi
    )
  }
}

object HOrderFuncTest extends App {
  val isEven = (i: Int) => i % 2 == 0
  val int    = Gen.choose(0, 1000)

  def genStringIntFn(g: Gen[Int]): Gen[String => Int] = g map (i => s => i)

  val takeWhileProp = Prop.forAll(SGen.listOf(int))(ns => ns.takeWhile(isEven).forall(isEven))

}
