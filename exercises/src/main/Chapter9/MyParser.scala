package main.Chapter9
import MyParserTypes._

object MyParserTypes {
  type Parser[+A] = Location => Result[A]

  sealed trait Result[+A]
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError) extends Result[Nothing]

}

object MyParsers extends Parsers[Parser] {
  def string(s: String): Parser[String] = (loc: Location) =>
    if (loc.input.startsWith(s, loc.offset))
      Success(s, s.length)
    else
      Failure(loc.toError(s"Expected: $s"))
  
}
