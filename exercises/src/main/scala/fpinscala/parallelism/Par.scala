package fpinscala.parallelism

import java.util.concurrent.TimeUnit.MILLISECONDS
import java.util.concurrent._

import fpinscala.parallelism.Par.{lazyUnit, map2, unit}

import language.implicitConversions
import scala.collection.immutable.List.empty
import scala.concurrent.duration.FiniteDuration

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => es.submit(() => a)

  // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  def lazyUnit[A](a: => A): Par[A] = (es: ExecutorService) => es.submit(() => a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  private case class CompositeFuture[A, B, C](af: Future[A], bf: Future[B])(cm: (A,B) => C) extends Future[C] {
    def cancel(mayInterruptIfRunning: Boolean): Boolean = af.cancel(mayInterruptIfRunning) && bf.cancel(mayInterruptIfRunning)
    def isCancelled: Boolean = af.isCancelled && bf.isCancelled
    def isDone: Boolean = af.isDone && bf.isDone
    def get(): C = cm(af.get(), bf.get())
    def get(timeout: Long, unit: TimeUnit): C = {
      val (a, duration) = evaluate(af.get(timeout, unit))
      val remaining = FiniteDuration(timeout, unit) - duration
      val b = bf.get(remaining.length, remaining.unit)

      cm(a, b)
    }

    private def evaluate[R](ex: => R):(R, FiniteDuration) = {
      val start = System.currentTimeMillis()
      val result = ex
      val end = System.currentTimeMillis() - start
      (result, FiniteDuration(end, MILLISECONDS))
    }
  }

  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af: Future[A] = a(es)
      val bf: Future[B] = b(es)
      CompositeFuture(af, bf)(f)
    }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldLeft(unit(empty[A]))((rp, p) => map2(rp, p)(_ :+ _))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = ???

  def fork[A](a: => Par[A]): Par[A] =
  // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
}

//object ParTest extends App {
//  val ctx = Executors.newSingleThreadExecutor()
//  val res = map2(unit(12), unit(22))(_ + _)(ctx).get()
//
//  println(res)
//
//  val res2 = Par.parMap(List(1, 2, 2, 3, 4, 5))(i => s"rr ${i + 1}")(ctx).get()
//
//  println(res2)
//
//  val a = lazyUnit((1 to 1000000).map(_ * 15).sum)
//  val S = Executors.newFixedThreadPool(1)
//
//  println(Par.equal(S)(a, Par.fork(a)))
//
//  S.shutdown()
//}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}
