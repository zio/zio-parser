package zio.parser.examples

import zio.Chunk
import zio.parser.{Syntax, _}
import zio.test.Assertion.{equalTo, isRight}
import zio.test._

object JsonExample extends ZIOSpecDefault {
  sealed abstract class Json
  object Json {
    final case class Obj(fields: Chunk[(String, Json)]) extends Json
    final case class Arr(elements: Chunk[Json])         extends Json
    final case class Bool(value: Boolean)               extends Json
    final case class Str(value: String)                 extends Json
    final case class Num(value: java.math.BigDecimal)   extends Json
    case object Null                                    extends Json
  }

  val whitespace: Syntax[String, Char, Char, Char]  = Syntax.charIn(' ', '\t', '\r', '\n')
  val whitespaces: Syntax[String, Char, Char, Unit] = whitespace.*.asPrinted((), Chunk(' '))

  val quote: Syntax[String, Char, Char, Unit]          = Syntax.char('\"')
  val escapedChar: Syntax[String, Char, Char, Char]    = Syntax.charNotIn('\"') // TODO: real escaping support
  val quotedString: Syntax[String, Char, Char, String] = (quote ~> escapedChar.*.string <~ quote)

  val nul: Syntax[String, Char, Char, Json.Null.type] = Syntax.string("null", Json.Null)

  val bool: Syntax[String, Char, Char, Json.Bool] =
    Syntax.string("true", Json.Bool(true)) <>
      Syntax.string("false", Json.Bool(false))

  val str: Syntax[String, Char, Char, Json.Str] = quotedString
    .transform(Json.Str.apply, (s: Json.Str) => s.value)

  val digits                                                                = Syntax.digit.repeat
  val signedIntStr: Syntax[String, Char, Char, (Option[Unit], Chunk[Char])] =
    Syntax.char('-').? ~ digits
  val frac: Syntax[String, Char, Char, Chunk[Char]]                         = Syntax.char('.') ~> digits
  val exp: Syntax[String, Char, Char, (Char, Char, Chunk[Char])]            =
    Syntax.charIn('e', 'E') ~ Syntax.charIn('+', '-') ~ digits
  val jsonNum: Syntax[String, Char, Char, String]                           = (signedIntStr ~ frac.? ~ exp.?).string

  val num: Syntax[String, Char, Char, Json.Num] = jsonNum
    .transform(
      s => Json.Num(BigDecimal(s).bigDecimal),
      (v: Json.Num) => v.value.toString()
    )

  val listSep: Syntax[String, Char, Char, Unit]       = Syntax.char(',').surroundedBy(whitespaces)
  lazy val list: Syntax[String, Char, Char, Json.Arr] =
    (Syntax.char('[') ~> json.repeatWithSep(listSep) <~ Syntax.char(']'))
      .transform(Json.Arr.apply, (arr: Json.Arr) => arr.elements)

  val keyValueSep: Syntax[String, Char, Char, Unit]             = Syntax.char(':').surroundedBy(whitespaces)
  lazy val keyValue: Syntax[String, Char, Char, (String, Json)] =
    (str ~ keyValueSep ~ json).transform[(String, Json)](
      { case (key, value) => (key.value, value) },
      { case (key, value) => (Json.Str(key), value) }
    )
  val obj: Syntax[String, Char, Char, Json.Obj]                 = (Syntax.char('{') ~>
    keyValue.repeatWithSep(listSep).surroundedBy(whitespaces) <~
    Syntax.char('}'))
    .transform(Json.Obj.apply, (arr: Json.Obj) => arr.fields)

  lazy val json: Syntax[String, Char, Char, Json] =
    nul.widen[Json] <> bool.widen[Json] <> str.widen[Json] <> num.widen[Json] <> list.widen[Json] <> obj.widen[Json]

//  Debug.printParserTree(json.asParser)
//  println("-----")
//  Debug.printParserTree(json.asParser.optimized)

  override def spec: ZSpec[Environment, Any] =
    suite("JSON example")(
      parsingTests("parsing with auto-backtrack", json.autoBacktracking),
      parsingTests("parsing with manual-backtrack", json.manualBacktracking)
    )

  def parsingTests(
      name: String,
      json: Syntax[String, Char, Char, Json]
  ): Spec[Any, TestFailure[Nothing], TestSuccess] =
    suite(name)(
      test("null") {
        assert(json.parseString("null"))(
          isRight(equalTo(Json.Null))
        )
      },
      test("true") {
        assert(json.parseString("true"))(
          isRight(equalTo(Json.Bool(true)))
        )
      },
      test("123") {
        assert(json.parseString("123"))(
          isRight(equalTo(Json.Num(BigDecimal(123).bigDecimal)))
        )
      },
      test("string") {
        assert(json.parseString("\"hello world\""))(
          isRight(equalTo(Json.Str("hello world")))
        )
      },
      test("array") {
        assert(json.parseString("[1, null, 3]"))(
          isRight(
            equalTo(
              Json.Arr(Chunk(Json.Num(BigDecimal(1).bigDecimal), Json.Null, Json.Num(BigDecimal(3).bigDecimal)))
            )
          )
        )
      },
      test("obj") {
        assert(json.parseString("""{ "x": 0, "hello": "world", "y": true, "z": [1, 2, 3] }"""))(
          isRight(
            equalTo(
              Json.Obj(
                Chunk(
                  "x"     -> Json.Num(BigDecimal(0).bigDecimal),
                  "hello" -> Json.Str("world"),
                  "y"     -> Json.Bool(true),
                  "z"     -> Json.Arr(
                    Chunk(
                      Json.Num(BigDecimal(1).bigDecimal),
                      Json.Num(BigDecimal(2).bigDecimal),
                      Json.Num(BigDecimal(3).bigDecimal)
                    )
                  )
                )
              )
            )
          )
        )
      }
    )
}
