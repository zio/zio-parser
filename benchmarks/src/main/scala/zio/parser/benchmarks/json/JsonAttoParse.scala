package zio.parser.benchmarks.json

import atto._, Atto._
import cats.implicits._
import zio.Chunk

// Based on https://github.com/typelevel/cats-parse/blob/main/bench/src/main/scala/cats/parse/bench/atto.scala
object JsonAttoParse extends Whitespace {
  // Bracketed, comma-separated sequence, internal whitespace allowed
  def seq[A](open: Char, p: Parser[A], close: Char): Parser[List[A]] =
    char(open).t ~> sepByT(p, char(',')) <~ char(close)

  // Colon-separated pair, internal whitespace allowed
  lazy val pair: Parser[(String, Json)] =
    pairByT(stringLiteral, char(':'), jexpr)

  // Json Expression
  lazy val jexpr: Parser[Json] = delay {
    stringLiteral -| Json.Str.apply |
      seq('{', pair, '}') -| (pairs => Json.Obj(Chunk.fromIterable(pairs))) |
      seq('[', jexpr, ']') -| (values => Json.Arr(Chunk.fromIterable(values))) |
      double -| (s => Json.Num(BigDecimal(s))) |
      string("null") >| Json.Null |
      string("true") >| Json.Bool(true) |
      string("false") >| Json.Bool(false)
  }

}

// Some extre combinators and syntax for coping with whitespace. Something like this might be
// useful in core but it needs some thought.
trait Whitespace {

  // Syntax for turning a parser into one that consumes trailing whitespace
  implicit class TokenOps[A](self: Parser[A]) {
    def t: Parser[A] =
      self <~ takeWhile(c => c.isSpaceChar || c === '\n')
  }

  // Delimited list
  def sepByT[A](a: Parser[A], b: Parser[_]): Parser[List[A]] =
    sepBy(a.t, b.t)

  // Delimited pair, internal whitespace allowed
  def pairByT[A, B](a: Parser[A], delim: Parser[_], b: Parser[B]): Parser[(A, B)] =
    pairBy(a.t, delim.t, b)

}
