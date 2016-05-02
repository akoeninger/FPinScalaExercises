package main.Chapter9

import language.higherKinds
import language.implicitConversions
import scala.util.matching.Regex

import main.Chapter8._

trait Parsers[Parser[+ _]] {
  self => // so inner classes may call methods of trait

  val numA: Parser[Int] = charCount('a')

  def orString(s1: String, s2: String): Parser[String]

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  // Leave succeed abstract, to avoid circular definition with map which uses flatMap and succeed
  def succeed[A](a: A): Parser[A]

  def defaultSucceed[A](a: A): Parser[A] = string("").map(_ => a)

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def slice[A](p: Parser[A]): Parser[String]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0)
      succeed(List())
    else
      p.map2(listOfN(n - 1, p))(_ :: _)

  run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")

  def many[A](p: Parser[A]): Parser[List[A]] =
    p.map2(wrap(many(p)))(_ :: _) or succeed(Nil)

  def many1[A](p: Parser[A]): Parser[List[A]] = p.map2(wrap(many(p)))(_ :: _)

  def map[A, B](pa: Parser[A])(f: A => B): Parser[B] = flatMap(pa)(f andThen succeed)

  def flatMap[A, B](a: Parser[A])(f: A => Parser[B]): Parser[B]

  def map2ViaProduct[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    product(p, p2) map f.tupled

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = for {
    a ← p
    b ← p2
  } yield (a, b)

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = for {
    a ← p
    b ← p2
  } yield f(a, b)

  def charCount(c: Char): Parser[Int] = char('a').many.map(_.size)

  run(charCount('a'))("") == Right(0)
  run(charCount('a'))("aa") == Right(2)
  run(charCount('a'))("bcc") == Right(0)

  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  // Return number of 'a'
  def regexParser: Parser[Int] = for {
    digit ← "[0-9]+".r
    n = digit.toInt
    _ ← listOfN(n, char('a'))
  } yield n

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def wrap[A](p: => Parser[A]): Parser[A]

  def whitespace: Parser[String] = "//s*".r

  def digits: Parser[String] = "//d*".r

  def word: Parser[String] = "//w*".r

  def letter: Parser[String] = "[A-Z]|[a-z]".r

  def leftBrace: Parser[Char] = char('{')

  def rightBrace: Parser[Char] = char('}')

  def thru(s: String): Parser[String] = s".*?${Regex.quote(s)}".r

  // Parses everything upto, but not including the given string
  def thruExclusive(s: String): Parser[String] = thru(s).map(_.init)

  def opt[A](p: Parser[A]): Parser[Option[A]] = p.map(Some(_)) | succeed(None)

  def surround[A](left: Parser[Any], right: Parser[Any])(p: => Parser[A]): Parser[A] =
    skipR(skipL(left, p), right)

  def deliminate1[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    map2(p, many1(skipL(p2, p)))(_ :: _)

  def deliminate[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    deliminate1(p, p2) | succeed[List[A]](Nil)

  def skipL[A](p: => Parser[Any], p2: Parser[A]): Parser[A] =
    map2(slice(p), p2)((_, b) => b)

  def skipR[A](p: Parser[A], p2: => Parser[Any]): Parser[A] =
    map2(p, slice(p2))((a, _) => a)

  def quoted: Parser[String] = string("\"") *> thruExclusive("\"")

  def doubleString: Parser[String] =
    token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)

  def double: Parser[Double] = doubleString.map(_.toDouble)

  def escapedQuoted: Parser[String] = {
    // TODO: Figure out implementation, need error handling
    quoted
  }

  def attempt[A](p: Parser[A]): Parser[A]

  def token[A](p: Parser[A]): Parser[A] = attempt(p) <* whitespace

  def opL[A](p: Parser[A])(op: Parser[(A, A) => A]): Parser[A] = map2(p, many(op ** p)) { (h, t) =>
    t.foldLeft(h)((a, b) => b._1(a, b._2))
  }

  def eof: Parser[String] = regex("\\z".r)

  def root[A](p: Parser[A]): Parser[A] = p <* eof

  implicit def string(s: String): Parser[String]

  implicit def regex(r: Regex): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps(p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def wrap: Parser[A] = self.wrap(p)

    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def listOfN(n: Int): Parser[List[A]] = self.listOfN(n, p)

    def many: Parser[List[A]] = self.many(p)

    def many1: Parser[List[A]] = self.many1(p)

    def slice: Parser[String] = self.slice(p)

    def *>[B](p2: => Parser[B]): Parser[B] = self.skipL(p, p2)

    def <*(p2: => Parser[Any]): Parser[A] = self.skipR(p, p2)

    def label(msg: String): Parser[A] = self.label(msg)(p)

    def scope(msg: String): Parser[A] = self.scope(msg)(p)

    def as[B](b: B): Parser[B] = self.map(self.slice(p))(_ ⇒ b)

    def surround(start: Parser[Any], stop: Parser[Any]): Parser[A] =
      self.surround(start, stop)(p)

    def sep(separator: Parser[Any]) = self.deliminate(p, separator)

    def sep1(separator: Parser[Any]) = self.deliminate1(p, separator)

    def token: Parser[A] = self.token(p)

    def attempt: Parser[A] = self.attempt(p)

    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)

    def run(input: String): Either[ParseError, A] = self.run(p)(input)
  }

  object Laws {
    def charIdentity(c: Char): Boolean = char(c).run(c.toString) == Right(c)

    def stringId(s: String): Boolean = run(string(s))(s) == Right(s)

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p map identity)(in)

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def productLaw[A, B, C](a: Parser[A], b: Parser[B], c: Parser[C])(in: Gen[String]): Prop =
      equal((a ** b) ** c map unbiasL, a ** (b ** c) map unbiasR)(in)

    def unbiasL[A, B, C](p: ((A, B), C)): (A, B, C) = (p._1._1, p._1._2, p._2)

    def unbiasR[A, B, C](p: (A, (B, C))): (A, B, C) = (p._1, p._2._1, p._2._2)

    def productMapLaw[A, B, C, D](
      a: Parser[A],
      b: Parser[B],
      f: A => C,
      g: B => D
    )(
      in: Gen[String]
    ): Prop = equal(
      product(a map f, b map g),
      product(a, b) map (ab => (f(ab._1), g(ab._2)))
    )(in)
  }

}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col  = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError = ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line - 1).next
    else ""
}

case class ParseError(
  stack: List[(Location, String)] = Nil,
  otherFailures: List[ParseError] = Nil
)