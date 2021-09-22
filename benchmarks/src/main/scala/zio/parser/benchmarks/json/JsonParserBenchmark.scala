package zio.parser.benchmarks.json

import atto.Parser
import cats.parse.Parser0
import fastparse.P
import org.http4s.parsley.Parsley
import zio.parser.Syntax
import zio.parser.benchmarks.{ParserBenchmark, ParserBenchmarkTestRunner, Parserz}

import scala.io.Source
import scala.util.Try

/** JSON benchmark suite from cats-parse
  */
abstract class JsonParserBenchmark(fileName: String) extends ParserBenchmark[Json] {
  override def loadInput(): String =
    Source.fromResource(fileName).getLines().mkString("\n")

  override final val zioSyntax: Syntax[String, Char, Char, Json, Json] = JsonZioParser.json

  override final val catParser: Parser0[Json] = JsonCatsParse.parser

  override final def fastParseP[_: P]: P[Json] = JsonFastParse.jsonExpr

  override final val attoParser: Parser[Json] = JsonAttoParse.jexpr

  override final def runParboiledParser(input: String): Try[Json] = {
    val parser = new JsonParboiled(input)
    parser.JSON.run()
  }

  override final val parsley: Parsley[Json] = JsonParsley.json

  override final val parserz: Parserz.Grammar[Any, Nothing, String, Json] =
    JsonParserz.js
}

class BarBench  extends JsonParserBenchmark("bar.json")
object BarBench extends ParserBenchmarkTestRunner[Json, BarBench] {
  override val self: ParserBenchmark[Json] = new BarBench

  override def resultToString(value: Json): String = "..."
}

// TODO: fix atto failure
class Qux2Bench  extends JsonParserBenchmark("qux2.json")
object Qux2Bench extends ParserBenchmarkTestRunner[Json, Qux2Bench] {
  override val self: ParserBenchmark[Json] = new Qux2Bench

  override def resultToString(value: Json): String = "..."
}

// TODO: fix parserz failure
class Bla25Bench  extends JsonParserBenchmark("bla25.json")
object Bla25Bench extends ParserBenchmarkTestRunner[Json, Bla25Bench] {
  override val self: ParserBenchmark[Json] = new Bla25Bench

  override def resultToString(value: Json): String = "..."
}

// TODO: fix atto, zio and cats-parse failures
class CountriesBench  extends JsonParserBenchmark("countries.geo.json")
object CountriesBench extends ParserBenchmarkTestRunner[Json, CountriesBench] {
  override val self: ParserBenchmark[Json] = new CountriesBench

  override def resultToString(value: Json): String = "..."
}

class Ugh10kBench  extends JsonParserBenchmark("ugh10k.json")
object Ugh10kBench extends ParserBenchmarkTestRunner[Json, Ugh10kBench] {
  override val self: ParserBenchmark[Json] = new Ugh10kBench

  override def resultToString(value: Json): String = "..."
}
