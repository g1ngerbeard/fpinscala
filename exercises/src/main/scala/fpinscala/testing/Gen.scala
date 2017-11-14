package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.state.RNG.{Simple, boundedInt, flatMap, int}
import fpinscala.state.State.Rand

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

  type FailedCase = String
  type SuccessCount = Int

  type TestCases = Int

  case class Prop(run: (TestCases, RNG) => Result) {
    def &&(that: Prop): Prop = Prop {
      (n, rng) => {
        val r1 = run(n, rng)
        if (r1.isFalsified) r1 else that.run(n, rng)
      }
    }

    def ||(that: Prop): Prop = Prop {
      (n, rng) => {
        val r1 = run(n, rng)
        if (r1.isFalsified) that.run(n, rng) else r1
      }
    }
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =

    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

case class Gen[A](sample: State[RNG, A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.map(f).flatMap(_.sample))

  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => Gen.listOfN(n, this))

  def unsized: SGen[A] = SGen(_ => this)

}

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(State(boundedInt(start, stopExclusive)))

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def double: Gen[Double] = Gen(State(RNG.double))

  def pair[A](g: Gen[A]): Gen[(A, A)] = Gen(listOfN(2, g).sample.map { case Seq(a, b) => (a, b) })

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](ga: Gen[A], gb: Gen[A]): Gen[A] = boolean.flatMap(if (_) ga else gb)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val (gen1, w1) = g1
    val (gen2, w2) = g2
    val p = w1 / (w1 + w2)

    double.flatMap(d => if (d < p) gen1 else gen2)
  }

}

case class SGen[+A](forSize: Int => Gen[A]) {

  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen(
    s => forSize(s).flatMap(a => f(a).forSize(s))
  )

  def map[B](f: A => B): SGen[B] = SGen(forSize(_).map(f))

}

object SGen {

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(Gen.listOfN(_, g))

//  listOf1

}

//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}

//trait SGen[+A] {
//
//}

object PropTest extends App {
  val rnd = Simple(Random.nextLong())

  val g1 = Gen(State(RNG.double))
  val g2 = Gen.unit(0.07)

  val gu = Gen.weighted((g1, 0.1), (g2, 1.2))

  println(Gen.listOfN(50, gu).sample.run(rnd))

}

