package main.Chapter9

import language.higherKinds
import language.implicitConversions

trait Parsers[ParserError, Parser[+_]] { self => // so inner classes may call methods of trait

  def char(c: Char): Parser[Char]
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def orString(s1: String, s2: String): Parser[String]
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def listOfN(n: Int): Parser[List[A]] = self.listOfN(n, p)

    def run(input: String): Either[ParseError, A] = self.run(p)(input)
  }

  object Laws {
    def charIdentity(c: Char): Boolean = char(c).run(c.toString) == Right(c)
    def stringId(s: String): Boolean = run(string(s))(s) == Right(s)

  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
  otherFailures: List[ParseError] = List()) {
}
