package zio.parser.benchmarks.basic
import zio.parser.benchmarks.{ParserBenchmark, ParserBenchmarkTestRunner, Parserz}

import scala.util.Try

/** Simple basic benchmark parsing (any.repeat1).map(case class)
  */
class RepeatAnyChar extends ParserBenchmark[Chars] {
  import RepeatAnyChar._

  override final def loadInput(): String = "hello" * 1000

  override final val zioSyntax: zio.parser.Syntax[String, Char, Char, Chars, Chars] = {
    import zio.Chunk
    import zio.parser._

    Syntax.anyChar.repeat
      .transform[Chars, Chars](
        chunk => Chars(chunk.toSeq),
        chars => Chunk.fromIterable(chars.chars)
      )
      .manualBacktracking
  }

  override final val catParser: cats.parse.Parser[Chars] = {
    import cats.parse._
    Parser.anyChar.rep.map(nel => Chars(nel.toList))
  }

  override final def fastParseP[P: fastparse.P]: fastparse.P[Chars] = {
    import fastparse._
    import NoWhitespace._

    SingleChar.rep.map(Chars.apply)
  }

  override final val attoParser: atto.Parser[Chars] = {
    import atto._
    import Atto._

    many1(anyChar).map(nel => Chars(nel.toList))
  }

  override final def runParboiledParser(input: String): Try[Chars] = {
    import org.parboiled2._
    val parser = new ParboiledTest(input)
    parser.InputLine.run()
  }

  override final val parsley: org.http4s.parsley.Parsley[Chars] = {
    import org.http4s.parsley._
    import org.http4s.parsley.Parsley._
    import org.http4s.parsley.Combinator._

    many(Char.anyChar).map(Chars.apply)
  }

  override final val parserz: Parserz.Grammar[Any, Nothing, String, Chars] = {
    import Parserz._
    import Parserz.Grammar._

    val anyChar: Grammar[Any, Nothing, String, Char] = consume(
      {
        case c :: cs => Right(cs -> c)
        case Nil     => Left("eoi")
      },
      { case (cs, c) =>
        Right(c :: cs)
      }
    )
    anyChar.rep1.map(
      Chars.apply,
      _.chars.toList.asInstanceOf[::[Char]]
    )
  }
}

case class Chars(chars: Seq[Char]) {
  override def toString: String = chars.mkString
}

object RepeatAnyChar extends ParserBenchmarkTestRunner[Chars, RepeatAnyChar] {
  override val self: ParserBenchmark[Chars] = new RepeatAnyChar

  class ParboiledTest(val input: org.parboiled2.ParserInput) extends org.parboiled2.Parser {
    import org.parboiled2._

    def InputLine = rule(Chs ~ EOI)
    def Chs       = rule(capture(oneOrMore(ANY)) ~> ((s: String) => Chars(s)))
  }
}
