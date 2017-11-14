package fpinscala.state

import fpinscala.state.RNG.Rand


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (Int.MinValue, r) => nonNegativeInt(r)
    case (i, r) if i < 0 => (-1 * i, r)
    case p => p
  }

  val double: Rand[Double] = map(nonNegativeInt)(i => i.toDouble / Int.MaxValue)

  def intDouble: Rand[(Int, Double)] = map2(int, double)(Tuple2.apply)

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)

    ((d1, d2, d3), r3)
  }

  //  def ints(count: Int)(rng: RNG): (List[Int], RNG) = sequence(List.fill(count)(int))(rng)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rnd => {
    val (a, rndA) = ra(rnd)
    val (b, rndB) = rb(rndA)

    (f(a, b), rndB)
  }

  //  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => fs match {
  //    case rand :: rest =>
  //      val (a, newR) = rand(rng)
  //      val (l, lastR) = sequence(rest)(newR)
  //      (a :: l, lastR)
  //
  //    case Nil => (Nil, rng)
  //  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rngA) = f(rng)
    g(a)(rngA)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0) (mod, _)
    else nonNegativeLessThan(n)
  }
}

case class State[S, +A](run: S => (A, S)) {

  import State._

  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, ns) = run(s)
    f(a).run(ns)
  })
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {

  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = State(s => fs match {
    case rand :: rest =>
      val (a, newR) = rand.run(s)
      val (l, lastR) = sequence(rest).run(newR)
      (a :: l, lastR)

    case Nil => (Nil, s)
  })
//
//  def modify[S](f: S => S): State[S, Unit] = for {
//    s <- get
//    _ <- set(f(s))
//  } yield ()
//
//  def get[S]: State[S, S] = State(s => (s, s))
//
//  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
//
//  def updateState: (Input, Machine) => Machine = {
//    case (_, m@Machine(_, 0, _)) => m
//    case (Coin, Machine(true, candies, coins)) => Machine(false, candies, coins + 1)
//    case (Turn, Machine(false, candies, coins)) => Machine(true, candies - 1, coins)
//    case (_, m) => m
//  }
//
//  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
//    inputs
//      .map(unit[Machine, Input])
//      .foldLeft(get[Machine])((rs, is) => is.map2(rs)(updateState))
//      .flatMap {
//        case m@Machine(_, candies, coins) => set(m).map(_ => (candies, coins))
//      }
//  }

}

object StateTest extends App {

  import State._
  import RNG._

  val simple: Simple = Simple(924155179)

  //  println(nonNegativeInt(simple))
  //
  //  println(double(Simple(1513545579)))
  //
  //  val (list, _) = ints(10)(Simple(593545579))
  //
  //  println(list)
  //
  //  val (value, _) = nonNegativeLessThan(10000)(simple)
  //  println(value)

  println(State(nonNegativeInt).map(_.toString).run(simple)._1)

  println(State(nonNegativeInt).map2(State(nonNegativeInt))((i1, i2) => s"$i1 + $i2").run(simple)._1)

  println(sequence(List.fill(10)(State(int))).run(simple)._1)

  println(List(1).sum)


//  println(simulateMachine(List(Coin, Turn, Turn, Coin, Coin, Turn)).run(Machine(locked = true, 10, 0)))
}