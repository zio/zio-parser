package zio.parser.benchmarks.json

import zio.parser.benchmarks.Parserz
import Parserz.Grammar._
import Parserz.Expr._
import Parserz._
import zio.Chunk

// Based on https://github.com/spartanz/parserz/blob/master/src/test/scala/org/spartanz/parserz/compare/ParserzJsonTest.scala
object JsonParserz {
  type S    = Unit
  type E    = String
  type G[A] = Grammar[Any, Nothing, E, A]

  def char(c: Char): G[Char]              = consume(
    cs => if (cs.nonEmpty && cs.head == c) Right((cs.tail, c)) else Left("expected: " + c),
    { case (cs, _) => Right(c :: cs) }
  )
  def token(t: List[Char]): G[List[Char]] = consume(
    cs => if (cs.startsWith(t)) Right((cs.drop(t.length), t)) else Left("expected: " + t),
    { case (cs, _) => Right(t reverse_::: cs) }
  )

  val dot: G[Char]      = char('.')
  val comma: G[Char]    = char(',')
  val colon: G[Char]    = char(':')
  val quote: G[Char]    = char('"')
  val bracket1: G[Char] = char('[')
  val bracket2: G[Char] = char(']')
  val brace1: G[Char]   = char('{')
  val brace2: G[Char]   = char('}')

  val spacing: G[Unit] = consumePure(
    cs => (cs.dropWhile(c => c == ' ' || c == '\n' || c == '\r'), ()),
    { case (cs, _) => ' ' :: cs }
  )

  val ch: G[Char] = consume(
    cs => if (cs.nonEmpty) Right((cs.tail, cs.head)) else Left("expected: char"),
    { case (cs, c) => Right(c :: cs) }
  )

  def chars(cond: Char => Boolean): G[List[Char]] = consumePure(
    { cs =>
      val out = cs.takeWhile(cond)
      (cs.drop(out.length), out)
    },
    { case (cs, cs1) =>
      cs1 reverse_::: cs
    }
  )

  val digits: G[List[Char]]     = chars(c => '0' <= c && c <= '9')
  val sign: G[Option[Char]]     = ch.filter("expected: +/-")(in('+', '-')).option
  val exponent: G[List[Char]]   = (ch.filter("expected: E")(in('e', 'E')) ~ sign, ('E', Some('+'))) ~> digits
  val fractional: G[List[Char]] = (dot, '.') ~> digits
  val integral: G[List[Char]]   = digits

  val num: G[Json.Num] = (sign ~ integral ~ fractional.orEmpty ~ exponent.orEmpty).map(
    { case (((s, l1), l2), l3) => Json.Num((s.mkString + l1.mkString + l2.mkString + l3.mkString).toDouble) },
    { case Json.Num(_) => ??? }
  )

  val `null`: G[Json.Null.type] = token("null".toList).map(_ => Json.Null, _ => "null".toList)
  val `false`: G[Json.Bool]     = token("false".toList).map(_ => Json.Bool(false), _ => "false".toList)
  val `true`: G[Json.Bool]      = token("true".toList).map(_ => Json.Bool(true), _ => "true".toList)

  val string: G[String] =
    ((spacing ~ quote, ((), '"')) ~> chars(c => c != '\"' && c != '\\') <~ ('"', quote)).map(_.mkString, _.toList)
  val str: G[Json.Str]  = string.map(Json.Str, "\"" + _.value + "\"")

  val arr: G[Json.Arr] =
    ((bracket1, '[') ~> js.separated(comma).map(_.values, { _: List[Json] => ??? }) <~ (((), ']'), spacing ~ bracket2))
      .map(
        lst => Json.Arr(Chunk.fromIterable(lst)),
        arr => arr.elements.toList
      )

  val field: G[(String, Json)] = (string <~ (':', colon)) ~ js

  val obj: G[Json.Obj] =
    ((brace1, '{') ~> field.separated(comma).map(_.values, { _: List[(String, Json)] => ??? }) <~ ((
      (),
      '}'
    ), spacing ~ brace2)).map(
      lst => Json.Obj(Chunk.fromIterable(lst)),
      obj => obj.fields.toList
    )

  // todo: smash ?
  lazy val js: G[Json] = delay {
    ((spacing, ()) ~> (obj | arr | str | `true` | `false` | `null` | num) <~ ((), spacing)).map(
      {
        case Left(Left(Left(Left(Left(Left(v))))))  => v
        case Left(Left(Left(Left(Left(Right(v)))))) => v
        case Left(Left(Left(Left(Right(v)))))       => v
        case Left(Left(Left(Right(v))))             => v
        case Left(Left(Right(v)))                   => v
        case Left(Right(v))                         => v
        case Right(v)                               => v
      },
      {
        case j: Json.Obj      => Left(Left(Left(Left(Left(Left(j))))))
        case j: Json.Arr      => Left(Left(Left(Left(Left(Right(j))))))
        case j: Json.Str      => Left(Left(Left(Left(Right(j)))))
        case Json.Bool(true)  => Left(Left(Left(Right(Json.Bool(true)))))
        case Json.Bool(false) => Left(Left(Right(Json.Bool(false))))
        case Json.Null        => Left(Right(Json.Null))
        case j: Json.Num      => Right(j)
      }
    )
  }
}
