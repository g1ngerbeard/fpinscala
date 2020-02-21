package fpinscala.parsing

import fpinscala.testing.Prop._
import fpinscala.testing._

import language.higherKinds

trait Parsers[Parser[+_]] {
  self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def map[A, B](a: Parser[A])(f: A => B): Parser[B]

  def map2[A, B, C](p: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] = (p ** p2) map f.tupled

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, p.many)((head, tail) => head :: tail) or succeed(List.empty)

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, p.many) { case (_, b) => b }

  def slice[A](p: Parser[A]): Parser[String]

  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  implicit def char(c: Char): Parser[Char]

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def product[A, B](p: Parser[A], p2: Parser[B]): Parser[(A, B)]

  def zeroOrMore(head: Char, tail: Char): Parser[(Int, Int)]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def many: Parser[List[A]] = self.many(p)

    def many1: Parser[List[A]] = self.many1(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def slice: Parser[String] = self.slice(p)

  }

  object Laws {

    //    run(or(string("abra"), string("cadabra")))("abra") == Right("abra")
    //    run(or(string("abra"), string("cadabra")))("cadabra") == Right("cadabra")
    //
    //    run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
    //    run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab")
    //    run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab")

    //    map(p)(a => a) == p

    //    val numA: Parser[Int] = char('a').many.map(_.size)

    //    run(numA)("aaa") == Right(3)
    //    run(numA)("b") == Right(0)

    val manyAmanyB: Parser[(Int, Int)] = char('a').many.slice.map(_.length) ** char('b').many1.slice.map(_.length)

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p, p.map(a => a))(in)

    def productLaw[A](p1: Parser[A], p2: Parser[A]): Prop = ???

    run(succeed(1) ** succeed(2))("123") == Right((1, 2))

    //    def productLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(, p ** p)(in)

  }

}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String = ???
//    if (input.length > 1) input.lines.skip(line - 1)
//    else ""
}

case class ParseError(stack: List[(Location, String)] = List(), otherFailures: List[ParseError] = List()) {}
