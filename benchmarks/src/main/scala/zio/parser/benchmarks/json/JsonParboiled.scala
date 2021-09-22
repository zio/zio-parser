package zio.parser.benchmarks.json

import scala.annotation.switch
import org.parboiled2._
import zio.Chunk

// Based on https://github.com/typelevel/cats-parse/blob/main/bench/src/main/scala/cats/parse/bench/parboiled2.scala
class JsonParboiled(val input: ParserInput) extends Parser with StringBuilding {
  import CharPredicate.{Digit, Digit19, HexDigit}
  import JsonParboiled._

  // the root rule
  def JSON = rule(WhiteSpace ~ Value ~ EOI)

  def JsonObject: Rule1[Json.Obj] =
    rule {
      ws('{') ~ zeroOrMore(Pair).separatedBy(ws(',')) ~ ws('}') ~> ((fields: Seq[(String, Json)]) =>
        Json.Obj(Chunk.fromIterable(fields))
      )
    }

  def Pair = rule(JsonStringUnwrapped ~ ws(':') ~ Value ~> ((_, _)))

  def Value: Rule1[Json] =
    rule {
      // as an optimization of the equivalent rule:
      //   JsonString | JsonNumber | JsonObject | JsonArray | JsonTrue | JsonFalse | JsonNull
      // we make use of the fact that one-char lookahead is enough to discriminate the cases
      run {
        (cursorChar: @switch) match {
          case '"'                                                             => JsonString
          case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-' => JsonNumber
          case '{'                                                             => JsonObject
          case '['                                                             => JsonArray
          case 't'                                                             => JsonTrue
          case 'f'                                                             => JsonFalse
          case 'n'                                                             => JsonNull
          case _                                                               => MISMATCH
        }
      }
    }

  def JsonString = rule(JsonStringUnwrapped ~> (Json.Str(_)))

  def JsonStringUnwrapped = rule('"' ~ clearSB() ~ Characters ~ ws('"') ~ push(sb.toString))

  def JsonNumber = rule(
    capture(Integer ~ optional(Frac) ~ optional(Exp)) ~> (s => Json.Num(BigDecimal(s))) ~ WhiteSpace
  )

  def JsonArray = rule(
    ws('[') ~ zeroOrMore(Value).separatedBy(ws(',')) ~ ws(']') ~> (values => Json.Arr(Chunk.fromIterable(values)))
  )

  def Characters = rule(zeroOrMore(NormalChar | '\\' ~ EscapedChar))

  def NormalChar = rule(!QuoteBackslash ~ ANY ~ appendSB())

  def EscapedChar =
    rule(
      QuoteSlashBackSlash ~ appendSB()
        | 'b' ~ appendSB('\b')
        | 'f' ~ appendSB('\f')
        | 'n' ~ appendSB('\n')
        | 'r' ~ appendSB('\r')
        | 't' ~ appendSB('\t')
        | Unicode ~> { code => sb.append(code.asInstanceOf[Char]); () }
    )

  def Unicode = rule(
    'u' ~ capture(HexDigit ~ HexDigit ~ HexDigit ~ HexDigit) ~> (java.lang.Integer.parseInt(_, 16))
  )

  def Integer = rule(optional('-') ~ (Digit19 ~ Digits | Digit))

  def Digits = rule(oneOrMore(Digit))

  def Frac = rule("." ~ Digits)

  def Exp = rule(ignoreCase('e') ~ optional(anyOf("+-")) ~ Digits)

  def JsonTrue = rule("true" ~ WhiteSpace ~ push(Json.Bool(true)))

  def JsonFalse = rule("false" ~ WhiteSpace ~ push(Json.Bool(false)))

  def JsonNull = rule("null" ~ WhiteSpace ~ push(Json.Null))

  def WhiteSpace = rule(zeroOrMore(WhiteSpaceChar))

  def ws(c: Char) = rule(c ~ WhiteSpace)
}

object JsonParboiled {
  val WhiteSpaceChar      = CharPredicate(" \n\r\t\f")
  val QuoteBackslash      = CharPredicate("\"\\")
  val QuoteSlashBackSlash = QuoteBackslash ++ "/"
}
