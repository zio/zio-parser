package zio.parser.benchmarks

import zio.parser.benchmarks.basic.RepeatAnyChar

import scala.util.Try

class RepeatSpecificChars extends ParserBenchmark[String] {
  override final def loadInput(): String = "abcdefghijklmnop" * 10000

  override final val zioSyntax: zio.parser.Syntax[String, Char, Char, String, String] = {
    import zio.parser._
    Syntax.charIn('a' to 'p': _*).atLeast(10000).string
  }

  override final val catParser: cats.parse.Parser[String] = {
    import cats.parse._
    (Parser.charIn('a' to 'p')).rep(10000).string
  }

  override final def fastParseP[P: fastparse.P]: fastparse.P[String] = null

  override final val attoParser: atto.Parser[String] = null

  override final def runParboiledParser(input: String): Try[String] = null

  override final val parsley: org.http4s.parsley.Parsley[String] = null

  override final val parserz: Parserz.Grammar[Any, Nothing, String, String] = null
}

case class Chars(chars: Seq[Char]) {
  override def toString: String = chars.mkString
}

object RepeatSpecificChars extends ParserBenchmarkTestRunner[String, RepeatSpecificChars] {
  override val self: ParserBenchmark[String] = new RepeatSpecificChars
}
