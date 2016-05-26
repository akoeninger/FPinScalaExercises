package main.Chapter9
import scala.util.matching.Regex

import MyParserTypes._

object MyParserTypes {
  type Parser[+A] = Location => Result[A]

  sealed trait Result[+A] {
    def uncommit: Result[A] = this match {
      case Failure(get, true) => Failure(get, false)
      case _ => this
    }

    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Success(get, charsConsumed) => this
      case Failure(e) => Failure(f(e))
    }
  }

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

  /** Returns -1 if s1.startsWith(s2), otherwise returns the
      * first index where the two strings differed. If s2 is
      * longer than s1, returns s1.length. */
    def firstNonMatchingIndex(s1: String, s2: String, offset: Int): Int = {
      var i = 0
      while (i < s1.length && i < s2.length) {
        if (s1.charAt(i+offset) != s2.charAt(i)) return i
        i += 1
      }
      if (s1.length - offset >= s2.length) -1
      else s1.length - offset
    }

}

object MyParsers extends Parsers[ParseError, Parser] {

  override def attempt[A](p: Parser[A]): Parser[A] = loc => p(loc).uncommit

  override def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = s => s1(s) match {
    case Failure(get, false) => s2(s)
    case r => r
  }

  override def string(s: String): Parser[String] = (loc: Location) => {
    val pointOfDiff = firstNonMatchingIndex(loc.input, s, loc.offset)
    if (pointOfDiff == -1)
      Success(s, s.length)
    else
      Failure(loc.toError(s"Expected: $s, point of diff: $pointOfDiff"))
  } mapError(_.label(s"Failed to match $s"))

  override def succeed[A](a: A): Parser[A] = (loc: Location) =>
    Success(a, 0) // Not sure what charsConsumed should be

  override def slice[A](p: Parser[A]): Parser[String] = (loc: Location) => p(loc) match {
    case Success(get, charsConsumed) => Success(loc.input.substring(loc.offset, loc.offset + charsConsumed), charsConsumed)
    case f@Failure(get) => f
  }

  override implicit def regex(r: Regex): Parser[String] = (loc: Location) =>
    r.findPrefixOf(loc.input) match {
      case Some(x) => Success(x, x.length)
      case None => Failure(loc.toError(s"regex $r"))
    }

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] = loc =>
    p(loc).mapError(_.push(loc, msg))

  override def label[A](msg: String)(p: Parser[A]): Parser[A] = loc =>
    p(loc).mapError(_.label(msg))
}
