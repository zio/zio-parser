package zio.parser.benchmarks.json

import org.http4s.parsley._
import org.http4s.parsley.Parsley._
import org.http4s.parsley.Combinator._
import zio.Chunk

// Based on https://github.com/typelevel/cats-parse/blob/main/bench/src/main/scala/cats/parse/bench/parsley.scala
object JsonParsley {

  def json: Parsley[Json] = {
    val jsontoks                  = LanguageDef(
      "",
      "",
      "",
      false,
      NotRequired,
      NotRequired,
      NotRequired,
      NotRequired,
      Set.empty,
      Set.empty,
      true,
      Predicate(Char.isWhitespace)
    )
    val tok                       = new TokenParser(jsontoks)
    lazy val obj: Parsley[Json]   = tok.braces(
      tok.commaSep(+(tok.stringLiteral <~> tok.colon *> value)).map(pairs => Json.Obj(Chunk.fromIterable(pairs)))
    )
    lazy val array: Parsley[Json] =
      tok.brackets(tok.commaSep(value)).map(list => Json.Arr(Chunk.fromIterable(list)))
    lazy val value: Parsley[Json] =
      (tok.stringLiteral.map(Json.Str.apply)
        <|> tok.symbol("true") *> Parsley.pure(Json.Bool(true))
        <|> tok.symbol("false") *> Parsley.pure(Json.Bool(false))
        <|> tok.symbol("null") *> Parsley.pure(Json.Null)
        <|> array
        <|> attempt(tok.float).map(value => Json.Num(BigDecimal(value)))
        <|> tok.integer.map(i => Json.Num(BigDecimal(i)))
        <|> obj)

    tok.whiteSpace *> (obj <|> array) <* eof
  }
}
