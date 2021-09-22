package zio.parser.benchmarks.json

import fastparse._, NoWhitespace._
import zio.Chunk

// Based on https://github.com/typelevel/cats-parse/blob/main/bench/src/main/scala/cats/parse/bench/fastparse.scala
object JsonFastParse {
  def stringChars(c: Char) = c != '\"' && c != '\\'

  def space[_: P]      = P(CharsWhileIn(" \r\n", 0))
  def digits[_: P]     = P(CharsWhileIn("0-9"))
  def exponent[_: P]   = P(CharIn("eE") ~ CharIn("+\\-").? ~ digits)
  def fractional[_: P] = P("." ~ digits)
  def integral[_: P]   = P("0" | CharIn("1-9") ~ digits.?)

  def number[_: P] =
    P(CharIn("+\\-").? ~ integral ~ fractional.? ~ exponent.?).!.map(x => Json.Num(BigDecimal(x)))

  def `null`[_: P]  = P("null").map(_ => Json.Null)
  def `false`[_: P] = P("false").map(_ => Json.Bool(false))
  def `true`[_: P]  = P("true").map(_ => Json.Bool(true))

  def hexDigit[_: P]      = P(CharIn("0-9a-fA-F"))
  def unicodeEscape[_: P] = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)
  def escape[_: P]        = P("\\" ~ (CharIn("\"/\\\\bfnrt") | unicodeEscape))

  def strChars[_: P] = P(CharsWhile(stringChars))
  def string[_: P]   =
    P(space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(Json.Str.apply)

  def array[_: P] =
    P("[" ~/ jsonExpr.rep(sep = ","./) ~ space ~ "]").map(values => Json.Arr(Chunk.fromIterable(values)))

  def pair[_: P] = P(string.map(_.value) ~/ ":" ~/ jsonExpr)

  def obj[_: P] =
    P("{" ~/ pair.rep(sep = ","./) ~ space ~ "}").map(pairs => Json.Obj(Chunk.fromIterable(pairs)))

  def jsonExpr[_: P]: P[Json] = P(
    space ~ (obj | array | string | `true` | `false` | `null` | number) ~ space
  )
}
