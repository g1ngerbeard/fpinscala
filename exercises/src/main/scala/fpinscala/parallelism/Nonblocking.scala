package fpinscala.parallelism

import java.util.concurrent.{
  Callable,
  CountDownLatch,
  ExecutorService,
  Executors
}
import java.util.concurrent.atomic.AtomicReference

import fpinscala.parallelism.Nonblocking.Par.{choiceN, lazyUnit, run}

import language.implicitConversions
import scala.util.Random

object Nonblocking {

  trait Future[+A] {
    private[parallelism] def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  object Par {

    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref = new java.util.concurrent.atomic.AtomicReference[A] // A mutable, threadsafe reference, to use for storing the result
      val latch = new CountDownLatch(1) // A latch which, when decremented, implies that `ref` has the result
      // Asynchronously set the result, and decrement the latch
      p(es) { a =>
        ref.set(a); latch.countDown()
      }

      // Block until the `latch.countDown` is invoked asynchronously
      latch.await()
      // Once we've passed the latch, we know `ref` has been set, and return its value
      ref.get
    }

    def unit[A](a: A): Par[A] = es => (cb: A => Unit) => cb(a)

    /** A non-strict version of `unit` */
    def delay[A](a: => A): Par[A] = es => (cb: A => Unit) => cb(a)

    def fork[A](a: => Par[A]): Par[A] =
      es => (cb: A => Unit) => eval(es)(a(es)(cb))

    /**
      * Helper function for constructing `Par` values out of calls to non-blocking continuation-passing-style APIs.
      * This will come in handy in Chapter 13.
      */
    def async[A](f: (A => Unit) => Unit): Par[A] = es => (k: A => Unit) => f(k)

    /**
      * Helper function, for evaluating an action
      * asynchronously, using the given `ExecutorService`.
      */
    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] {
        def call: Unit = r
      })

    def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
      es =>
        (cb: C => Unit) => {
          var ar: Option[A] = None
          var br: Option[B] = None
          val combiner = Actor[Either[A, B]](es) {
            case Left(a) =>
              if (br.isDefined) eval(es)(cb(f(a, br.get)))
              else ar = Some(a)
            case Right(b) =>
              if (ar.isDefined) eval(es)(cb(f(ar.get, b)))
              else br = Some(b)
          }
          p(es)(a => combiner ! Left(a))
          p2(es)(b => combiner ! Right(b))
      }

    // specialized version of `map`
    def map[A, B](p: Par[A])(f: A => B): Par[B] =
      es =>
        (cb: B => Unit) =>
          p(es)(a =>
            eval(es) {
              cb(f(a))
          })

    def lazyUnit[A](a: => A): Par[A] =
      fork(unit(a))

    def asyncF[A, B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
      as match {
        case Nil    => unit(Nil)
        case h :: t => map2(h, fork(sequence(t)))(_ :: _)
      }

    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l, r) = as.splitAt(as.length / 2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

    def sequence[A](as: List[Par[A]]): Par[List[A]] =
      map(sequenceBalanced(as.toIndexedSeq))(_.toList)

    // exercise answers

    /*
     * We can implement `choice` as a new primitive.
     *
     * `p(es)(result => ...)` for some `ExecutorService`, `es`, and
     * some `Par`, `p`, is the idiom for running `p`, and registering
     * a callback to be invoked when its result is available. The
     * result will be bound to `result` in the function passed to
     * `p(es)`.
     *
     * If you find this code difficult to follow, you may want to
     * write down the type of each subexpression and follow the types
     * through the implementation. What is the type of `p(es)`? What
     * about `t(es)`? What about `t(es)(cb)`?
     */
    def choice[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      es =>
        new Future[A] {
          def apply(cb: A => Unit): Unit =
            p(es) { b =>
              if (b) eval(es) {
                t(es)(cb)
              } else
                eval(es) {
                  f(es)(cb)
                }
            }
      }

    def choiceN[A](p: Par[Int])(ps: List[Par[A]]): Par[A] =
      es => (cb: A => Unit) => eval(es)(p(es)(i => eval(es)(ps(i)(es)(cb))))

    def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A],
                                             ifFalse: Par[A]): Par[A] =
      choiceN(map(a)(if (_) 1 else 0))(List(ifFalse, ifTrue))

    def choiceMap[K, V](p: Par[K])(ps: Map[K, Par[V]]): Par[V] = chooser(p)(ps)

    // see `Nonblocking.scala` answers file. This function is usually called something else!

    def chooser[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
      es => (cb: B => Unit) => eval(es)(p(es)(a => eval(es)(f(a)(es)(cb))))

    def flatMap[A, B](p: Par[A])(f: A => Par[B]): Par[B] = chooser(p)(f)

    def choiceViaChooser[A](p: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] =
      chooser(p)(if (_) f else t)

    def choiceNChooser[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
      chooser(p)(choices)

    def join[A](p: Par[Par[A]]): Par[A] = es => (cb: A => Unit) => eval(es)(eval(es)(cb))

    def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] = flatMap(a)(identity)

    def flatMapViaJoin[A, B](p: Par[A])(f: A => Par[B]): Par[B] = join(map(p)(f))

    /* Gives us infix syntax for `Par`. */
    implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

    // infix versions of `map`, `map2`
    class ParOps[A](p: Par[A]) {
      def map[B](f: A => B): Par[B] = Par.map(p)(f)

      def map2[B, C](b: Par[B])(f: (A, B) => C): Par[C] = Par.map2(p, b)(f)

      def zip[B](b: Par[B]): Par[(A, B)] = p.map2(b)((_, _))
    }

  }

}

object ParTest extends App {

  val c1 = lazyUnit((1 to 1000).map(_ * 2).sum)

  val c2 = lazyUnit(s"Result${2 + 3}")

  val c3 = lazyUnit("Some:text:is:written:here".split(':').map(_.length).toList)

  val ch = lazyUnit(Random.nextInt(3))

  val par = Nonblocking.Par.choiceNChooser(ch)(List(c1, c2, c3))

  val result = run(Executors.newFixedThreadPool(10))(par)

  println(result.toString)

}
