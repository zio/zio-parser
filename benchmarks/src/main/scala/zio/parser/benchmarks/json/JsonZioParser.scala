package zio.parser.benchmarks.json

import zio.Chunk
import zio.parser._

import scala.util.Try

object JsonZioParser {
  // TODO: this is not a proper JSON parser yet, to be fixed (numbers, escaping)

  val whitespaces = Syntax.charIn(' ', '\t', '\r', '\n').*.unit(Chunk(' '))

  val quote       = Syntax.char('\"')
  val escapedChar = Syntax.charNotIn('\"') // TODO: real escaping support

  val quotedString = (quote ~> escapedChar.*.string <~ quote)

  val nul = Syntax.string("null", Json.Null)

  val bool =
    Syntax.string("true", Json.Bool(true)) <>
      Syntax.string("false", Json.Bool(false))

  val str = quotedString
    .transform(Json.Str.apply, (s: Json.Str) => s.value)

  val digits       = Syntax.digit.repeat
  val signedIntStr = Syntax.char('-').? ~ digits
  val frac         = Syntax.char('.') ~> digits
  val exp          = Syntax.charIn('e', 'E') ~ Syntax.charIn('+', '-') ~ digits
  val jsonNum      = (signedIntStr ~ frac.? ~ exp.?).string

  val num = jsonNum
    .transform(
      s => Json.Num(BigDecimal(s)),
      (v: Json.Num) => v.value.toString()
    )

  val listSep   = Syntax.char(',').surroundedBy(whitespaces)
  lazy val list = (Syntax.char('[') ~> json.repeatWithSep0(listSep) <~ Syntax.char(']'))
    .transform(Json.Arr, (arr: Json.Arr) => arr.elements)

  val keyValueSep   = Syntax.char(':').surroundedBy(whitespaces)
  lazy val keyValue = (str ~ keyValueSep ~ json).transform[(String, Json)](
    { case (key, value) => (key.value, value) },
    { case (key, value) => (Json.Str(key), value) }
  )
  val obj           = (Syntax.char('{') ~>
    keyValue.repeatWithSep0(listSep).surroundedBy(whitespaces) <~
    Syntax.char('}'))
    .transform(Json.Obj, (arr: Json.Obj) => arr.fields)

  lazy val json: Syntax[String, Char, Char, Json] =
    (nul.widen[Json] <>
      bool.widen[Json] <>
      str.widen[Json] <>
      num.widen[Json] <>
      list.widen[Json] <>
      obj.widen[Json]).manualBacktracking
}
