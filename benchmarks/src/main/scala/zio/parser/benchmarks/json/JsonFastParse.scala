package zio.parser.benchmarks.json

import fastparse._, NoWhitespace._
import zio.Chunk

// Based on https://github.com/typelevel/cats-parse/blob/main/bench/src/main/scala/cats/parse/bench/fastparse.scala
object JsonFastParse {
  def stringChars(c: Char) = c != '\"' && c != '\\'

  def space[P0: P]      = P(CharsWhileIn(" \r\n", 0))
  def digits[P0: P]     = P(CharsWhileIn("0-9"))
  def exponent[P0: P]   = P(CharIn("eE") ~ CharIn("+\\-").? ~ digits)
  def fractional[P0: P] = P("." ~ digits)
  def integral[P0: P]   = P("0" | CharIn("1-9") ~ digits.?)

  def number[P0: P] =
    P(CharIn("+\\-").? ~ integral ~ fractional.? ~ exponent.?).!.map(x => Json.Num(BigDecimal(x)))

  def `null`[P0: P]  = P("null").map(_ => Json.Null)
  def `false`[P0: P] = P("false").map(_ => Json.Bool(false))
  def `true`[P0: P]  = P("true").map(_ => Json.Bool(true))

  def hexDigit[P0: P]      = P(CharIn("0-9a-fA-F"))
  def unicodeEscape[P0: P] = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)
  def escape[P0: P]        = P("\\" ~ (CharIn("\"/\\\\bfnrt") | unicodeEscape))

  def strChars[P0: P] = P(CharsWhile(stringChars))
  def string[P0: P]   =
    P(space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(Json.Str.apply)

  def array[P0: P] =
    P("[" ~/ jsonExpr.rep(sep = ","./) ~ space ~ "]").map(values => Json.Arr(Chunk.fromIterable(values)))

  def pair[P0: P] = P(string.map(_.value) ~/ ":" ~/ jsonExpr)

  def obj[P0: P] =
    P("{" ~/ pair.rep(sep = ","./) ~ space ~ "}").map(pairs => Json.Obj(Chunk.fromIterable(pairs)))

  def jsonExpr[P0: P]: P[Json] = P(
    space ~ (obj | array | string | `true` | `false` | `null` | number) ~ space
  )
}
