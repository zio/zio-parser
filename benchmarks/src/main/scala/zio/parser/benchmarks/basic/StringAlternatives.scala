package zio.parser.benchmarks.basic

import zio.Chunk
import zio.parser.benchmarks.{ParserBenchmark, ParserBenchmarkTestRunner, Parserz}
import zio.parser.internal.Debug

import scala.util.{Random, Try}

/** Parsing a sequence of variable length string tokens that cannot be distinguished by their first character
  */
class StringAlternatives extends ParserBenchmark[Tokens] {
  import StringAlternatives._

  override def loadInput(): String = {
    val N  = 1000
    val sb = new StringBuilder
    for (_ <- 1 to N)
      Random.between(0, 4) match {
        case 0 => sb.append("true")
        case 1 => sb.append("false")
        case 2 => sb.append("maybe")
        case 3 => sb.append("maybenot")
      }
    sb.toString()
  }

  override final val zioSyntax: zio.parser.Syntax[String, Char, Char, Tokens] = {
    import zio.parser._

    val t     = Syntax.string("true", True)
    val f     = Syntax.string("false", False)
    val m     = Syntax.string("maybe", Maybe)
    val mn    = Syntax.string("maybenot", MaybeNot).backtrack
    val token = t.widen[Token] | f.widen[Token] | mn.widen[Token] | m.widen[Token]
    token.repeat0
      .transform(
        (chunk: Chunk[Token]) => Tokens(chunk),
        (tks: Tokens) => Chunk.fromIterable(tks.tokens)
      )
      .manualBacktracking
  }

  override final val catParser: cats.parse.Parser0[Tokens] = {
    import cats.parse._

    val t  = Parser.string("true").as(True)
    val f  = Parser.string("false").as(False)
    val m  = Parser.string("maybe").as(Maybe)
    val mn = Parser.string("maybenot").as(MaybeNot)

    val token = t | f | mn.backtrack | m
    token.rep0.map(Tokens.apply)
  }

  override final def fastParseP[P: fastparse.P]: fastparse.P[Tokens] = {
    import fastparse._
    import NoWhitespace._

    def t  = P("true").map(_ => True)
    def f  = P("false").map(_ => False)
    def m  = P("maybe").map(_ => Maybe)
    def mn = P("maybenot").map(_ => MaybeNot)

    def token = t | f | mn | m
    token./.rep.map(Tokens.apply) ~ End
  }

  override final val attoParser: atto.Parser[Tokens] = {
    import atto._
    import Atto._

    val t  = string("true").map(_ => True)
    val f  = string("false").map(_ => False)
    val m  = string("maybe").map(_ => Maybe)
    val mn = string("maybenot").map(_ => MaybeNot)

    val token = t | f | mn | m
    many(token).map(Tokens.apply)
  }

  override final def runParboiledParser(input: String): Try[Tokens] = {
    val parser = new ParboiledTest(input)
    parser.InputLine.run()
  }

  override final val parsley: org.http4s.parsley.Parsley[Tokens] = {
    import org.http4s.parsley._
    import org.http4s.parsley.Parsley._
    import org.http4s.parsley.Combinator._

    val t  = Char.string("true").map(_ => True)
    val f  = Char.string("false").map(_ => False)
    val m  = Char.string("maybe").map(_ => Maybe)
    val mn = Char.string("maybenot").map(_ => MaybeNot)

    val token = t <|> f <|> attempt(mn) <|> m
    many(token).map(Tokens.apply)
  }

  override final val parserz: Parserz.Grammar[Any, Nothing, String, Tokens] = {
    import Parserz._
    import Parserz.Grammar._

    def token(t: List[Char]): Grammar[Any, Nothing, String, List[Char]] = consume(
      cs => if (cs.startsWith(t)) Right((cs.drop(t.length), t)) else Left("expected: " + t),
      { case (cs, _) => Right(t reverse_::: cs) }
    )

    val t  = token("true".toList).map(_ => True, (_: True.type) => "true".toList)
    val f  = token("false".toList).map(_ => False, (_: False.type) => "false".toList)
    val m  = token("maybe".toList).map(_ => Maybe, (_: Maybe.type) => "maybe".toList)
    val mn = token("maybenot".toList).map(_ => MaybeNot, (_: MaybeNot.type) => "maybenot".toList)

    val tok: Grammar[Any, Nothing, String, Token] =
      (t | f | mn | m).map[Token](
        {
          case Left(Left(Left(v)))  => v
          case Left(Left(Right(v))) => v
          case Left(Right(v))       => v
          case Right(v)             => v
        },
        {
          case True     => Left(Left(Left(True)))
          case False    => Left(Left(Right(False)))
          case MaybeNot => Left(Right(MaybeNot))
          case Maybe    => Right(Maybe)
        }
      )
    tok.rep.map(Tokens.apply, (tks: Tokens) => tks.tokens.toList)
  }
}

case class Tokens(tokens: Seq[Token]) {
  override def toString: String = tokens.mkString(", ")
}

sealed trait Token
case object True     extends Token
case object False    extends Token
case object Maybe    extends Token
case object MaybeNot extends Token

object StringAlternatives extends ParserBenchmarkTestRunner[Tokens, StringAlternatives] {
  override val self: ParserBenchmark[Tokens] = new StringAlternatives

  class ParboiledTest(val input: org.parboiled2.ParserInput) extends org.parboiled2.Parser {
    import org.parboiled2._

    def InputLine = rule(Toks ~ EOI)
    def Toks      = rule(oneOrMore(Tok) ~> Tokens)
    def Tok       = rule {
      capture(T | F | MN | M) ~> {
        case "true"     => True
        case "false"    => False
        case "maybe"    => Maybe
        case "maybenot" => MaybeNot
      }
    }
    def T         = rule("true")
    def F         = rule("false")
    def M         = rule("maybe")
    def MN        = rule("maybenot")
  }
}
