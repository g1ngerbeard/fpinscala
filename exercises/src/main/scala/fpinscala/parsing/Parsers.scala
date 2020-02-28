package fpinscala.parsing

import fpinscala.testing.Prop._
import fpinscala.testing._

import scala.util.matching.Regex

trait Parsers[Parser[+_]] {
  self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] = a.flatMap(a => succeed(f(a)))

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    for {
      a <- p
      b <- p2
    } yield f(a, b)

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  // todo: tailrec
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n > 0)
      map2(p, listOfN(n - 1, p))((a, listA) => a :: listA)
    else
      succeed(List.empty)

  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, p.many)((head, tail) => head :: tail) or succeed(List.empty)

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, p.many) { case (_, b) => b }

  def slice[A](p: Parser[A]): Parser[String]

  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  implicit def char(c: Char): Parser[Char]

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    for {
      a <- p
      b <- p2
    } yield (a, b)

  def zeroOrMore(head: Char, tail: Char): Parser[(Int, Int)]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  implicit def regex(r: Regex): Parser[String]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def many: Parser[List[A]] = self.many(p)

    def many1: Parser[List[A]] = self.many1(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def slice: Parser[String] = self.slice(p)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

  }

  object Laws {

    val singleDigitParser: Parser[Int] = "\\d".r.map(_.toInt)

    val contextParser: Parser[String] = for {
      n <- singleDigitParser
      seq <- listOfN(n, string("a"))
    } yield s"$n$seq"

    run(contextParser)("4aaaa") == Right("4aaaa")

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


trait JSON

object JSON {

  case object JNull extends JSON

  case class JNumber(get: Double) extends JSON

  case class JString(get: String) extends JSON

  case class JBool(get: Boolean) extends JSON

  case class JArray(get: IndexedSeq[JSON]) extends JSON

  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    // todo: take care of spaces and new lines
    val spaces = (char(' ') | char('\n')).many.slice

    val jnull: Parser[JSON] = string("null").map(_ => JNull)

    // todo: write this using more primitive parser combinators?
    val jnumber: Parser[JSON] = "-?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][+-]?\\d+)".r.slice.map(str => JNumber(str.toDouble))

    val stringLiteral = ("\"" ** ".*".r.slice ** "\"").map(_._1._2)

    val jstring: Parser[JSON] = stringLiteral.map(JString)

    val jbool: Parser[JSON] = ("true" | "false").map(str => JBool(str == "true"))

    lazy val jsobject: Parser[JSON] =
      ("{" ** (stringLiteral ** ":" ** anyJVal).map(t => t._1._1 -> t._2).many ** "}")
        .map(_._1._2.toMap)
        .map(JObject)

    lazy val jarray: Parser[JSON] =
      ("[" ** (anyJVal ** ",?".r).map(_._1).many ** "]")
        .map(res => JArray(res._1._2.toIndexedSeq))

    lazy val anyJVal: Parser[JSON] = jnull | jnumber | jbool | jstring | jarray | jsobject

    jsobject
  }
}
